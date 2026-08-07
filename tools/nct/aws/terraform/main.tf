# Manual-testing opentofu for the xray-front EC2 provision flow.
#
# This is NOT auto-rendered by `nct provision-aws` (that command is still
# TBD — see todo/nct.org Stage 6). It exists so a human can drive the
# AMI-import + instance-launch flow by hand against the VHD produced by
# `nct machine build-aws-image xray-front [--inject-key]`, end-to-end.
#
# Prereqs:
#   1. `nct machine build-aws-image xray-front --inject-key` → prints a VHD path.
#   2. Upload it to S3:
#        aws s3 cp <vhd> s3://<var.bucket>/<var.image_name>.vhd
#      (Content-address the key by nix store hash so re-uploads are cheap.)
#   3. Set the variables (terraform.tfvars or -var flags):
#        region, bucket, image_name, instance_type, key_name, ssh_pubkey
#   4. tofu init && tofu apply
#
# What this does:
#   - imports the S3 VHD as an AMI via `aws_ec2_import_image` (a long-running
#     job; `tofu apply` blocks until it completes — minutes).
#   - creates / reuses an EC2 keypair from `var.ssh_pubkey`.
#   - launches an instance from the imported AMI, allocates + associates an
#     Elastic IP, and opens 22/443 in a security group.
#   - prints the instance's public DNS for `clan machines update` targeting.
#
# The import-image resource is the slow, stateful part; the instance half is
# plain. See todo/nct.org Stage 6 Part B for the rationale + the open
# question of whether `nct provision-aws` should do the import via the aws
# CLI instead and only let tofu manage the instance.

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

variable "region" {
  type        = string
  description = "AWS region to launch into."
}

variable "bucket" {
  type        = string
  description = "S3 bucket holding the uploaded VHD."
}

variable "image_name" {
  type        = string
  description = "S3 object key (without extension) for the VHD; also used as the AMI name base."
}

variable "vhd_hash" {
  type        = string
  default     = ""
  description = "Content hash (e.g. nix store hash) of the VHD. Changing it forces a re-import."
}

variable "instance_type" {
  type        = string
  default     = "t3.micro"
  description = "EC2 instance type."
}

variable "key_name" {
  type        = string
  default     = "nixos-config-xray-front"
  description = "EC2 keypair name (created if var.ssh_pubkey is set and it doesn't exist)."
}

variable "ssh_pubkey" {
  type        = string
  default     = ""
  description = "SSH public key (openssh format) to register as the EC2 keypair. If empty, the keypair must already exist."
}

variable "associate_public_ip" {
  type        = bool
  default     = true
  description = "Associate a public IP (DHCP'd) with the instance. Set false if launching into a public subnet that assigns one."
}

provider "aws" {
  region = var.region
}

data "aws_caller_identity" "current" {}

data "aws_availability_zones" "available" {
  state = "available"
  filter {
    name   = "region-name"
    values = [var.region]
  }
}

# Default VPC + subnet for manual testing. For real deployments you'd pass
# explicit subnet/sg ids (see todo/nct.org Stage 6 open questions).
data "aws_vpc" "default" {
  default = true
}

data "aws_subnet" "default" {
  vpc_id            = data.aws_vpc.default.id
  availability_zone = data.aws_availability_zones.available.names[0]
}

# --- AMI import from the uploaded S3 VHD -------------------------------------
#
# `aws_ec2_import_image` is the newer resource that imports a VM as an AMI
# directly. It takes minutes (it's a VM Import Service job); `tofu apply`
# blocks until it completes. The `triggers` block forces a re-import when the
# VHD content changes (otherwise the resource is stale).
#
# The VM Import Service needs a role with the right trust + permissions.
# nixpkgs/cloudinit docs reference `vmimport`; we create it here for the
# manual-testing path so no out-of-band AWS setup is required.

resource "aws_iam_role" "vmimport" {
  name = "vmimport-nct"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = { Service = "vmie.amazonaws.com" }
      Action = "sts:AssumeRole"
      Condition = {
        StringEquals = { "sts:Externalid" = "vmimport" }
      }
    }]
  })
}

resource "aws_iam_role_policy" "vmimport" {
  name = "vmimport-nct-policy"
  role = aws_iam_role.vmimport.id
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:GetBucketLocation",
          "s3:GetObject",
          "s3:ListBucket",
        ]
        Resource = [
          "arn:aws:s3:::${var.bucket}",
          "arn:aws:s3:::${var.bucket}/*",
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "ec2:ModifySnapshotAttribute",
          "ec2:CopySnapshot",
          "ec2:RegisterImage",
          "ec2:Describe*",
        ]
        Resource = "*"
      },
    ]
  })
}

resource "aws_ec2_import_image" "xray_front" {
  description = "xray-front (nixos-config, ${var.image_name})"
  architecture = "x86_64"
  boot_mode    = "uefi"
  role_name    = aws_iam_role.vmimport.name

  disk_container {
    format = "vhd"
    s3 {
      s3_bucket = var.bucket
      s3_key    = "${var.image_name}.vhd"
    }
  }

  # Force re-import when the VHD content changes.
  triggers = {
    vhd_hash = var.vhd_hash != "" ? var.vhd_hash : var.image_name
  }

  tags = {
    Name    = "xray-front"
    Managed = "nct-build-aws-image"
  }
}

# --- Keypair (created from a pubkey if provided) -----------------------------

resource "aws_key_pair" "xray_front" {
  count      = var.ssh_pubkey != "" ? 1 : 0
  key_name   = var.key_name
  public_key = var.ssh_pubkey
}

# --- Security group: SSH + HTTPS ---------------------------------------------

resource "aws_security_group" "xray_front" {
  name        = "xray-front-nct"
  description = "xray-front manual-testing SG"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    description = "HTTPS (Reality front)"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# --- Instance + Elastic IP ---------------------------------------------------

resource "aws_instance" "xray_front" {
  ami           = aws_ec2_import_image.xray_front.image_id
  instance_type = var.instance_type
  subnet_id     = data.aws_subnet.default.id

  vpc_security_group_ids = [aws_security_group.xray_front.id]
  key_name               = var.key_name

  associate_public_ip_address = var.associate_public_ip

  root_block_device {
    volume_type = "gp3"
    volume_size = 12
    tags = {
      Name = "xray-front-root"
    }
  }

  tags = {
    Name    = "xray-front"
    Managed = "nct-build-aws-image"
  }
}

resource "aws_eip" "xray_front" {
  domain   = "vpc"
  instance = aws_instance.xray_front.id
  tags = {
    Name    = "xray-front"
    Managed = "nct-build-aws-image"
  }
}

# --- Outputs -----------------------------------------------------------------

output "instance_id" {
  value = aws_instance.xray_front.id
}

output "public_ip" {
  value = aws_eip.xray_front.public_ip
}

output "public_dns" {
  value       = aws_eip.xray_front.public_dns
  description = "Pass to: clan machines update xray-front --target-host root@<this>"
}

output "ami_id" {
  value = aws_ec2_import_image.xray_front.image_id
}
