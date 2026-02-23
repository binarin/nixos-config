"""ISO image operations for ncf."""

import os
import shutil
from pathlib import Path

from rich.console import Console

from .. import config
from ..external import run_command, ExternalToolError

console = Console()

# Default ISO output location
DEFAULT_OUTPUT_DIR = Path.home() / ".cache" / "nixos-config" / "master" / "iso-wifi"
DEFAULT_OUTPUT_FILE = "nixos-wifi.iso"

# WiFi configuration defaults
DEFAULT_SSID = "agares-guest"
WIFI_PASSWORD_FILE = "files/agares-guest.git-crypt"


def read_wifi_password(repo_root: Path) -> str:
    """Read WiFi password from git-crypt decrypted file."""
    password_file = repo_root / WIFI_PASSWORD_FILE
    if not password_file.exists():
        raise ExternalToolError(
            "wifi-password",
            f"WiFi password file not found: {password_file}\n"
            "Make sure git-crypt is unlocked.",
        )
    content = password_file.read_bytes()
    if content.startswith(b"\x00GITCRYPT"):
        raise ExternalToolError(
            "wifi-password",
            f"WiFi password file is encrypted: {password_file}\n"
            "Run 'git-crypt unlock' first.",
        )
    return content.decode("utf-8").strip()


def build_iso_with_wifi(
    repo_root: Path, ssid: str, password: str, output: Path
) -> Path:
    """Build the NixOS ISO image with WiFi credentials.

    Uses impure mode to pass credentials via environment variables.
    Returns the path to the built ISO file.
    """
    console.print("[bold]Building ISO image with WiFi credentials...[/bold]")

    result_link = repo_root / "iso-result"

    # Set environment variables for impure nix build
    env = os.environ.copy()
    env["WIFI_SSID"] = ssid
    env["WIFI_PASSWORD"] = password

    run_command(
        [
            "nix",
            "build",
            "--impure",
            f"{repo_root}#nixosConfigurations.iso.config.system.build.isoImage",
            "-j",
            "auto",
            "-o",
            str(result_link),
        ],
        cwd=repo_root,
        env=env,
    )

    # Find the ISO file in the result
    iso_files = list(result_link.rglob("*.iso"))
    if not iso_files:
        raise ExternalToolError("nix build", "No ISO file found in build result")

    iso_file = iso_files[0]
    console.print(f"[green]✓[/green] ISO built: {iso_file}")

    # Copy to output location
    output.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(iso_file, output)
    console.print(f"[green]✓[/green] WiFi-enabled ISO copied to: {output}")

    return output


def read_password_file(password_file: Path) -> str:
    """Read WiFi password from a file."""
    if not password_file.exists():
        raise ExternalToolError(
            "wifi-password",
            f"Password file not found: {password_file}",
        )
    content = password_file.read_bytes()
    if content.startswith(b"\x00GITCRYPT"):
        raise ExternalToolError(
            "wifi-password",
            f"Password file is encrypted: {password_file}\n"
            "Run 'git-crypt unlock' first.",
        )
    return content.decode("utf-8").strip()


def run_build_wifi(
    output: Path | None = None,
    ssid: str = DEFAULT_SSID,
    password: str | None = None,
    password_file: Path | None = None,
    dry_run: bool = False,
) -> None:
    """Build ISO with WiFi credentials embedded.

    Uses impure nix build to pass credentials via environment variables.
    Password can be provided via:
    - password argument (direct value)
    - password_file argument (path to file containing password)
    - default git-crypt file if neither is provided
    """
    repo_root = config.find_repo_root()

    # Determine output path
    if output is None:
        output = DEFAULT_OUTPUT_DIR / DEFAULT_OUTPUT_FILE

    # Determine password source for display
    if password:
        password_source = "CLI argument"
    elif password_file:
        password_source = f"file ({password_file})"
    else:
        password_source = f"file ({WIFI_PASSWORD_FILE})"

    console.print("[bold]Building WiFi-enabled ISO[/bold]")
    console.print(f"  SSID: {ssid}")
    console.print(f"  Password: {password_source}")
    console.print(f"  Output: {output}")
    console.print()

    if dry_run:
        console.print("[yellow]Dry run - would perform the following:[/yellow]")
        console.print("  1. Read WiFi password")
        console.print(
            "  2. Build ISO via nix build --impure with WIFI_SSID/WIFI_PASSWORD env vars"
        )
        console.print(f"  3. Copy ISO to {output}")
        return

    # Get WiFi password (from CLI, password file, or default file)
    if password is None:
        if password_file is not None:
            password = read_password_file(password_file)
        else:
            password = read_wifi_password(repo_root)

    # Build the ISO with WiFi credentials
    build_iso_with_wifi(repo_root, ssid, password, output)

    console.print()
    console.print("[bold green]Build complete![/bold green]")
