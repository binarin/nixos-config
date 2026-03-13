"""Proxmox API client using proxmoxer with SSH backend."""

import os
from pathlib import Path
from typing import Any

from proxmoxer import ProxmoxAPI


class ProxmoxClient:
    """Wrapper around proxmoxer for LXC container and VM operations."""

    def __init__(self, host: str, user: str = "root"):
        """Initialize Proxmox client with SSH paramiko backend.

        Uses ~/.ssh/ keys for authentication. SSH agent is explicitly disabled
        to work around paramiko bugs that can cause connection failures when
        SSH_AUTH_SOCK is set.

        The user parameter is the SSH user (typically 'root'), not the
        Proxmox API user (like 'root@pam').
        """
        # Clear SSH agent env vars to work around paramiko bugs.
        # Paramiko has known issues with SSH agent integration that can cause
        # connection failures or unexpected behavior. By clearing these vars,
        # paramiko falls back to key-file-based authentication.
        os.environ.pop("SSH_AUTH_SOCK", None)
        os.environ.pop("SSH_AGENT_PID", None)

        self.api = ProxmoxAPI(host, user=user, backend="ssh_paramiko")
        # Find the node matching the host we connected to
        nodes = self.api.nodes.get()
        if not nodes:
            raise RuntimeError(f"No nodes found on Proxmox host {host}")
        # Try to match host to a node name; fall back to first node
        self.node = nodes[0]["node"]
        for n in nodes:
            if n["node"] == host or host.startswith(n["node"]):
                self.node = n["node"]
                break
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

    def get_vm_ip(self, vmid: int) -> str | None:
        """Get VM IPv4 address from QEMU guest agent.

        Uses qm guest cmd via SSH to query the guest agent for network
        interfaces. Returns the first non-loopback IPv4 address found.

        Args:
            vmid: VM ID

        Returns:
            IPv4 address or None if not available yet
        """
        import json
        import subprocess

        try:
            result = subprocess.run(
                [
                    "ssh",
                    f"root@{self.host}",
                    "qm",
                    "guest",
                    "cmd",
                    str(vmid),
                    "network-get-interfaces",
                ],
                capture_output=True,
                text=True,
                timeout=10,
            )
            if result.returncode != 0:
                return None
            ifaces = json.loads(result.stdout)
            for iface in ifaces:
                if iface.get("name") == "lo":
                    continue
                for addr in iface.get("ip-addresses", []):
                    if addr.get("ip-address-type") == "ipv4":
                        return addr["ip-address"]
        except (subprocess.TimeoutExpired, json.JSONDecodeError, KeyError):
            return None
        return None

    def upload_snippet(self, storage: str, filename: str, content: str) -> None:
        """Upload content as a snippet file.

        Args:
            storage: Storage name (must have snippets content type enabled)
            filename: Name of the snippet file
            content: Content to write to the file
        """
        # Proxmox snippets are stored in the storage's snippets directory
        # We use the storage API to write the file
        import paramiko

        # Connect via SSH and write the file directly
        # This is more reliable than the API for snippets
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        # Explicitly disable SSH agent to work around paramiko bugs
        ssh.connect(self.host, username="root", allow_agent=False)

        try:
            # Get the storage path for snippets
            sftp = ssh.open_sftp()
            snippet_path = f"/var/lib/vz/snippets/{filename}"
            with sftp.file(snippet_path, "w") as f:
                f.write(content)
            sftp.close()
        finally:
            ssh.close()

    # ISO operations

    def list_isos(self, storage: str = "local") -> list[str]:
        """List ISO files in storage.

        Args:
            storage: Storage name (must have iso content type enabled)

        Returns:
            List of ISO filenames (just the filename, not the full path)
        """
        content = self.api.nodes(self.node).storage(storage).content.get(content="iso")
        return [item["volid"].split("/")[-1] for item in content]

    def iso_exists(self, storage: str, filename: str) -> bool:
        """Check if ISO exists in storage.

        Args:
            storage: Storage name
            filename: ISO filename to check

        Returns:
            True if ISO exists, False otherwise
        """
        isos = self.list_isos(storage)
        return filename in isos

    def upload_iso(self, storage: str, local_path: Path) -> None:
        """Upload ISO to Proxmox storage via pv + SSH with progress.

        Args:
            storage: Storage name (must have iso content type enabled)
            local_path: Path to local ISO file
        """
        import subprocess

        filename = local_path.name
        remote_path = f"/var/lib/vz/template/iso/{filename}"
        source_size = local_path.stat().st_size

        pv_cmd = ["pv", "-s", str(source_size), "-N", "Uploading ISO"]
        ssh_cmd = ["ssh", f"root@{self.host}", f"cat > {remote_path}"]

        with open(local_path, "rb") as f:
            pv_proc = subprocess.Popen(pv_cmd, stdin=f, stdout=subprocess.PIPE)
            ssh_proc = subprocess.Popen(ssh_cmd, stdin=pv_proc.stdout)
            if pv_proc.stdout:
                pv_proc.stdout.close()

            ssh_rc = ssh_proc.wait()
            pv_rc = pv_proc.wait()

        if ssh_rc != 0:
            raise RuntimeError(f"SSH upload failed with exit code {ssh_rc}")
        if pv_rc != 0:
            raise RuntimeError(f"pv failed with exit code {pv_rc}")

    def attach_iso(self, vmid: int, iso_path: str) -> None:
        """Attach ISO to VM's cdrom drive.

        Args:
            vmid: VM ID
            iso_path: ISO path in format "storage:iso/filename.iso"
                      e.g., "local:iso/nixos-installer.iso"
        """
        self.api.nodes(self.node).qemu(vmid).config.put(ide2=f"{iso_path},media=cdrom")

    def detach_iso(self, vmid: int) -> None:
        """Detach ISO from VM's cdrom drive.

        Args:
            vmid: VM ID
        """
        self.api.nodes(self.node).qemu(vmid).config.put(ide2="none,media=cdrom")

    def set_boot_order(self, vmid: int, order: str) -> None:
        """Set VM boot order.

        Args:
            vmid: VM ID
            order: Boot order string, e.g., "order=ide2;scsi0" or "order=scsi0"
        """
        self.api.nodes(self.node).qemu(vmid).config.put(boot=order)

    def reboot_vm(self, vmid: int) -> None:
        """Reboot a VM.

        Args:
            vmid: VM ID
        """
        self.api.nodes(self.node).qemu(vmid).status.reboot.post()
