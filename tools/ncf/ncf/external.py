"""External tool wrappers for ncf."""

import json
import subprocess
import shutil
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from rich.console import Console

console = Console()


@dataclass
class ToolRegistration:
    """A single registration of a tool by a module."""

    module: str
    purpose: str


@dataclass
class ExternalTool:
    """Description of an external tool dependency."""

    name: str
    required: bool = True
    registrations: list[ToolRegistration] = field(default_factory=list)

    def add_registration(self, module: str, purpose: str) -> None:
        """Add a registration from a module."""
        self.registrations.append(ToolRegistration(module=module, purpose=purpose))


# Global registry of external tool dependencies
_tool_registry: dict[str, ExternalTool] = {}


def register_tool(
    name: str, purpose: str, module: str | None = None, required: bool = True
) -> None:
    """Register an external tool dependency.

    Call this at module load time to declare tool dependencies.
    The test suite will verify all registered tools are available.

    Args:
        name: The executable name (as it appears in PATH)
        purpose: What the tool is used for in this module
        module: The module registering the tool (auto-detected if None)
        required: If True, ncf requires this tool; if False, it's optional
    """
    import inspect

    if module is None:
        # Auto-detect calling module
        frame = inspect.currentframe()
        if frame and frame.f_back:
            module = frame.f_back.f_globals.get("__name__", "unknown")
        else:
            module = "unknown"

    if name not in _tool_registry:
        _tool_registry[name] = ExternalTool(name=name, required=required)
    elif not required:
        # Don't downgrade required to optional
        pass
    else:
        # Upgrade optional to required if any registration requires it
        _tool_registry[name].required = True

    _tool_registry[name].add_registration(module, purpose)


def get_registered_tools() -> dict[str, ExternalTool]:
    """Get all registered external tools."""
    return _tool_registry.copy()


def get_required_tools() -> list[ExternalTool]:
    """Get all required external tools."""
    return [t for t in _tool_registry.values() if t.required]


def get_optional_tools() -> list[ExternalTool]:
    """Get all optional external tools."""
    return [t for t in _tool_registry.values() if not t.required]


# Register core tools used by external.py itself
register_tool("ssh-keygen", "SSH host key generation")
register_tool("age-keygen", "Age keypair generation")
register_tool("sops", "Secrets encryption/decryption")
register_tool("ssh-to-age", "Convert SSH keys to age keys")
register_tool("yamlfmt", "YAML file formatting")
register_tool("apg", "Password generation")
register_tool("deploy", "Deploy NixOS configurations via deploy-rs")


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
    env: Optional[dict[str, str]] = None,
) -> subprocess.CompletedProcess:
    """Run a command with error handling."""
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=capture_output,
            text=True,
            check=check,
            env=env,
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


def yamlfmt(file_path: Path) -> None:
    """Format a YAML file using yamlfmt."""
    run_command(["yamlfmt", str(file_path)])


def nix_fmt(cwd: Path) -> None:
    """Run nix fmt in the given directory."""
    run_command(["nix", "fmt"], cwd=cwd)


def apg_generate_password(length: int = 24, mode: str = "SNCL") -> str:
    """Generate a random password using apg.

    Args:
        length: Password length (default: 24)
        mode: Character classes - N=Numeric, C=Capital, L=Lowercase, S=Special
              (default: SNCL for all classes)

    Returns:
        The generated password (never printed to stdout/stderr)
    """
    result = run_command(
        ["apg", "-M", mode, "-n", "1", "-m", str(length), "-x", str(length)]
    )
    return result.stdout.strip()


def sops_set_value(file_path: Path, key_path: str, value: str) -> None:
    """Set a value in a sops-encrypted YAML file using sops --set.

    Args:
        file_path: Path to the sops-encrypted YAML file
        key_path: YAML path using '/' or '.' as separator (e.g., 'service/password')
        value: The value to set
    """
    # Convert path separators to sops format: ["key1"]["key2"]
    parts = [p for p in key_path.replace(".", "/").split("/") if p]
    if not parts:
        raise ExternalToolError("sops", f"Invalid key path: {key_path}")

    sops_path = "".join(f'["{p}"]' for p in parts)

    # sops --set expects: '["key"]["subkey"] "value"'
    # Use json.dumps to properly escape special characters in the value
    json_value = json.dumps(value)
    run_command(["sops", "--set", f"{sops_path} {json_value}", str(file_path)])
