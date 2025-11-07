variable "vm_name" {
  description = "Name of the virtual machine"
  type        = string
  default     = "debian-vm"
}

variable "memory" {
  description = "Memory allocation for the VM in MB"
  type        = number
  default     = 2048
}

variable "vcpu" {
  description = "Number of virtual CPUs"
  type        = number
  default     = 2
}

variable "disk_size" {
  description = "Disk size in bytes"
  type        = number
  default     = 21474836480  # 20GB
}

variable "static_ip" {
  description = "Static IP address for the VM"
  type        = string
  default     = "192.168.2.17"
}

variable "gateway" {
  description = "Gateway IP address"
  type        = string
  default     = "192.168.2.1"
}

variable "dns_servers" {
  description = "List of DNS servers"
  type        = list(string)
  default     = ["192.168.2.1"]
}

variable "network_name" {
  description = "Name of the libvirt network"
  type        = string
  default     = "br0"
}

variable "libvirt_uri" {
  description = "Libvirt connection URI"
  type        = string
  default     = "qemu+ssh://user@remote-host/system?known_hosts=/persist/home/binarin/.ssh/known_hosts.d/known_hosts"
}
