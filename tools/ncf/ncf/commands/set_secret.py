"""Set a secret in a sops-encrypted YAML file."""

from pathlib import Path

from rich.console import Console

from ..external import apg_generate_password, sops_set_value, ExternalToolError

console = Console()


def run(
    sops_file: Path,
    key_path: str,
    length: int = 24,
    mode: str = "SNCL",
    dry_run: bool = False,
) -> None:
    """Generate a random password and set it in a sops-encrypted YAML file.

    The password is never printed to stdout/stderr for security.

    Args:
        sops_file: Path to the sops-encrypted YAML file
        key_path: YAML path to the secret (e.g., 'service/password' or 'db.password')
        length: Password length (default: 24)
        mode: apg mode for character classes (default: SNCL)
        dry_run: If True, show what would be done without making changes
    """
    if not sops_file.exists():
        raise ExternalToolError("sops", f"File does not exist: {sops_file}")

    if dry_run:
        console.print(f"[yellow]Would generate {length}-char password (mode: {mode})")
        console.print(f"[yellow]Would set secret at: {key_path}")
        console.print(f"[yellow]In file: {sops_file}")
        return

    # Generate password (captured in variable, never printed)
    password = apg_generate_password(length=length, mode=mode)

    # Set the secret
    sops_set_value(sops_file, key_path, password)

    console.print(f"[green]Secret set at path: {key_path}")
