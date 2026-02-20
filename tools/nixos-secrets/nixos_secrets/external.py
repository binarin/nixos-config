"""External tool wrappers for nixos-secrets."""

import subprocess
import shutil
from pathlib import Path
from typing import Optional

from rich.console import Console

console = Console()


class ExternalToolError(Exception):
    """Error running an external tool."""

    def __init__(self, tool: str, message: str, returncode: int = 1):
        self.tool = tool
        self.returncode = returncode
        super().__init__(f"{tool}: {message}")


def run_command(
    cmd: list[str],
    cwd: Optional[Path] = None,
    capture_output: bool = True,
    check: bool = True,
) -> subprocess.CompletedProcess:
    """Run a command with error handling."""
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=capture_output,
            text=True,
            check=check,
        )
        return result
    except subprocess.CalledProcessError as e:
        stderr = e.stderr if e.stderr else ""
        raise ExternalToolError(cmd[0], stderr.strip(), e.returncode)
    except FileNotFoundError:
        raise ExternalToolError(cmd[0], f"Command not found: {cmd[0]}")


def check_tool_exists(tool: str) -> bool:
    """Check if a tool is available in PATH."""
    return shutil.which(tool) is not None


def ensure_tools_available() -> None:
    """Ensure all required external tools are available."""
    required_tools = ["ssh-keygen", "age-keygen", "sops", "ssh-to-age"]
    missing = [tool for tool in required_tools if not check_tool_exists(tool)]
    if missing:
        raise ExternalToolError(
            "tools",
            f"Missing required tools: {', '.join(missing)}. "
            "Make sure you're in the nix develop shell.",
        )


def ssh_keygen_generate_all(target_dir: Path) -> None:
    """Generate all SSH host key types using ssh-keygen -A.

    ssh-keygen -A creates keys in etc/ssh/ relative to the target,
    so we need to move them afterwards.
    """
    # ssh-keygen -A expects a directory structure with etc/ssh/
    etc_ssh = target_dir / "etc" / "ssh"
    etc_ssh.mkdir(parents=True, exist_ok=True)

    run_command(["ssh-keygen", "-A", "-f", str(target_dir)])

    # Move keys from etc/ssh/ to target_dir
    for key_file in etc_ssh.iterdir():
        dest = target_dir / key_file.name
        if not dest.exists():
            key_file.rename(dest)
        else:
            key_file.unlink()  # Remove if destination already exists

    # Clean up empty directories
    etc_ssh.rmdir()
    (target_dir / "etc").rmdir()


def age_keygen_generate(output_path: Path) -> str:
    """Generate an age keypair.

    Returns the public key.
    Note: age-keygen outputs the public key to stderr as a comment.
    """
    result = run_command(["age-keygen", "-o", str(output_path)])
    # Public key is in stderr as: "Public key: age1..."
    for line in result.stderr.split("\n"):
        if line.startswith("Public key:"):
            return line.split(": ", 1)[1].strip()

    raise ExternalToolError("age-keygen", "Could not extract public key from output")


def age_keygen_extract_public(private_key_path: Path) -> str:
    """Extract public key from an age private key file.

    This works on unencrypted private keys.
    """
    result = run_command(["age-keygen", "-y", str(private_key_path)])
    return result.stdout.strip()


def sops_encrypt_inplace(file_path: Path) -> None:
    """Encrypt a file in place with sops."""
    run_command(["sops", "encrypt", "-i", str(file_path)])


def sops_decrypt_to_stdout(file_path: Path) -> str:
    """Decrypt a sops file and return the content."""
    result = run_command(["sops", "decrypt", str(file_path)])
    return result.stdout


def ssh_to_age(ssh_public_key_path: Path) -> str:
    """Convert an SSH public key to an age public key."""
    result = run_command(["ssh-to-age", "-i", str(ssh_public_key_path)])
    return result.stdout.strip()


def is_sops_encrypted(file_path: Path) -> bool:
    """Check if a file is sops-encrypted."""
    if not file_path.exists():
        return False
    content = file_path.read_text()
    return "sops" in content and ("ENC[" in content or '"sops":' in content)
