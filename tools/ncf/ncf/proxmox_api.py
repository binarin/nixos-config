"""Proxmox API client using proxmoxer with SSH backend."""

from typing import Any

from proxmoxer import ProxmoxAPI


class ProxmoxClient:
    """Wrapper around proxmoxer for LXC container and VM operations."""

    def __init__(self, host: str, user: str = "root"):
        """Initialize Proxmox client with SSH paramiko backend.

        Uses SSH agent or ~/.ssh/ keys for authentication.
        The user parameter is the SSH user (typically 'root'), not the
        Proxmox API user (like 'root@pam').
        """
        self.api = ProxmoxAPI(host, user=user, backend="ssh_paramiko")
        # Get the first (usually only) node
        nodes = self.api.nodes.get()
        if not nodes:
            raise RuntimeError(f"No nodes found on Proxmox host {host}")
        self.node = nodes[0]["node"]
        self.host = host

    def container_exists(self, hostname: str) -> int | None:
        """Check if container with hostname exists. Returns VMID or None."""
        for ct in self.api.nodes(self.node).lxc.get():
            if ct.get("name") == hostname:
                return int(ct["vmid"])
        return None

    def get_next_vmid(self) -> int:
        """Get next available VMID from cluster."""
        return int(self.api.cluster.nextid.get())

    def get_container_config(self, vmid: int) -> dict[str, Any]:
        """Get container configuration."""
        return dict(self.api.nodes(self.node).lxc(vmid).config.get())

    def start_container(self, vmid: int) -> None:
        """Start a container."""
        self.api.nodes(self.node).lxc(vmid).status.start.post()

    def stop_container(self, vmid: int) -> None:
        """Stop a container."""
        self.api.nodes(self.node).lxc(vmid).status.stop.post()

    def delete_container(self, vmid: int, purge: bool = True) -> None:
        """Delete a container."""
        self.api.nodes(self.node).lxc(vmid).delete(purge=1 if purge else 0)

    # VM operations

    def vm_exists(self, hostname: str) -> int | None:
        """Check if VM with hostname exists. Returns VMID or None."""
        for vm in self.api.nodes(self.node).qemu.get():
            if vm.get("name") == hostname:
                return int(vm["vmid"])
        return None

    def get_vm_config(self, vmid: int) -> dict[str, Any]:
        """Get VM configuration."""
        return dict(self.api.nodes(self.node).qemu(vmid).config.get())

    def start_vm(self, vmid: int) -> None:
        """Start a VM."""
        self.api.nodes(self.node).qemu(vmid).status.start.post()

    def stop_vm(self, vmid: int) -> None:
        """Stop a VM."""
        self.api.nodes(self.node).qemu(vmid).status.stop.post()

    def delete_vm(self, vmid: int, purge: bool = True) -> None:
        """Delete a VM."""
        self.api.nodes(self.node).qemu(vmid).delete(purge=1 if purge else 0)

    def upload_snippet(self, storage: str, filename: str, content: str) -> None:
        """Upload content as a snippet file.

        Args:
            storage: Storage name (must have snippets content type enabled)
            filename: Name of the snippet file
            content: Content to write to the file
        """
        # Proxmox snippets are stored in the storage's snippets directory
        # We use the storage API to write the file
        import io
        import paramiko

        # Connect via SSH and write the file directly
        # This is more reliable than the API for snippets
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh.connect(self.host, username="root")

        try:
            # Get the storage path for snippets
            sftp = ssh.open_sftp()
            snippet_path = f"/var/lib/vz/snippets/{filename}"
            with sftp.file(snippet_path, "w") as f:
                f.write(content)
            sftp.close()
        finally:
            ssh.close()
