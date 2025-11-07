output "vm_name" {
  description = "Name of the created virtual machine"
  value       = libvirt_domain.debian_vm.name
}

output "vm_id" {
  description = "ID of the created virtual machine"
  value       = libvirt_domain.debian_vm.id
}

output "static_ip" {
  description = "Static IP address assigned to the VM"
  value       = var.static_ip
}

output "ssh_command" {
  description = "SSH command to connect to the VM"
  value       = "ssh binarin@${var.static_ip}"
}

output "vm_memory" {
  description = "Memory allocated to the VM (MB)"
  value       = libvirt_domain.debian_vm.memory
}

output "vm_vcpu" {
  description = "Number of vCPUs allocated to the VM"
  value       = libvirt_domain.debian_vm.vcpu
}
