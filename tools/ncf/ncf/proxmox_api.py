"""Proxmox API client using proxmoxer with SSH backend."""

from typing import Any

from proxmoxer import ProxmoxAPI


class ProxmoxClient:
    """Wrapper around proxmoxer for LXC container operations."""

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
