terraform {
  required_providers {
    libvirt = {
      source  = "dmacvicar/libvirt"
      version = "~> 0.7"
    }
  }
}

provider "libvirt" {
  uri = var.libvirt_uri
}

resource "libvirt_pool" "default" {
  name = "default"
  type = "dir"
  target {
    path = "/var/lib/libvirt/images"
  }
}

resource "libvirt_network" "br0" {
  name      = "br0"
  mode      = "bridge"
  bridge    = "br0"
  autostart = true
}

resource "libvirt_volume" "debian_base" {
  name   = "debian-13-genericcloud-amd64.qcow2"
  pool   = libvirt_pool.default.name
  source = "https://cloud.debian.org/images/cloud/trixie/latest/debian-13-genericcloud-amd64.qcow2"
  format = "qcow2"
}

resource "libvirt_volume" "debian_vm" {
  name           = var.vm_name
  base_volume_id = libvirt_volume.debian_base.id
  pool           = libvirt_pool.default.name
  size           = var.disk_size
}

resource "libvirt_cloudinit_disk" "commoninit" {
  name      = "${var.vm_name}-commoninit.iso"
  pool      = libvirt_pool.default.name

  user_data = templatefile("${path.module}/cloud-init.cfg", {
    ssh_authorized_keys = compact(split("\n", file("/etc/ssh/authorized_keys.d/binarin")))
  })

  network_config = templatefile("${path.module}/network-config.cfg", {
    static_ip   = var.static_ip
    gateway     = var.gateway
    dns_servers = join(",", var.dns_servers)
  })
}

resource "libvirt_domain" "debian_vm" {
  name      = var.vm_name
  memory    = var.memory
  vcpu      = var.vcpu
  autostart = true

  cloudinit = libvirt_cloudinit_disk.commoninit.id

  network_interface {
    network_name   = var.network_name
    wait_for_lease = true
    addresses      = [var.static_ip]
  }

  disk {
    volume_id = libvirt_volume.debian_vm.id
  }

  console {
    type        = "pty"
    target_type = "serial"
    target_port = "0"
  }

  graphics {
    type        = "spice"
    listen_type = "address"
    autoport    = true
  }
}
