"""ISO image operations for ncf."""

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


def build_iso(repo_root: Path) -> Path:
    """Build the NixOS ISO image.

    Returns the path to the built ISO file.
    """
    console.print("[bold]Building ISO image...[/bold]")

    result_link = repo_root / "iso-result"

    run_command(
        [
            "nix",
            "build",
            f"{repo_root}#nixosConfigurations.iso.config.system.build.isoImage",
            "-j",
            "auto",
            "-o",
            str(result_link),
        ],
        cwd=repo_root,
    )

    # Find the ISO file in the result
    iso_files = list(result_link.rglob("*.iso"))
    if not iso_files:
        raise ExternalToolError("nix build", "No ISO file found in build result")

    iso_file = iso_files[0]
    console.print(f"[green]✓[/green] ISO built: {iso_file}")
    return iso_file


def read_wifi_password(repo_root: Path) -> str:
    """Read WiFi password from git-crypt decrypted file."""
    password_file = repo_root / WIFI_PASSWORD_FILE
    if not password_file.exists():
        raise ExternalToolError(
            "wifi-password",
            f"WiFi password file not found: {password_file}\n"
            "Make sure git-crypt is unlocked.",
        )
    return password_file.read_text().strip()


def inject_wifi(
    iso_file: Path, ssid: str, password: str, output_file: Path, repo_root: Path
) -> None:
    """Inject WiFi credentials into the ISO image."""
    console.print(f"[bold]Injecting WiFi credentials (SSID: {ssid})...[/bold]")

    script_path = repo_root / "scripts" / "inject-iso-wifi.sh"
    if not script_path.exists():
        raise ExternalToolError(
            "inject-iso-wifi.sh", f"Injection script not found: {script_path}"
        )

    # Ensure output directory exists
    output_file.parent.mkdir(parents=True, exist_ok=True)

    run_command(
        [str(script_path), str(iso_file), ssid, password, str(output_file)],
        cwd=repo_root,
        capture_output=False,  # Let the script output to console
    )

    console.print(f"[green]✓[/green] WiFi-enabled ISO created: {output_file}")


def run_build_wifi(
    output: Path | None = None,
    ssid: str = DEFAULT_SSID,
    dry_run: bool = False,
) -> None:
    """Build ISO with WiFi credentials injected.

    This combines the ISO build and WiFi injection into a single command.
    """
    repo_root = config.find_repo_root()

    # Determine output path
    if output is None:
        output = DEFAULT_OUTPUT_DIR / DEFAULT_OUTPUT_FILE

    console.print(f"[bold]Building WiFi-enabled ISO[/bold]")
    console.print(f"  SSID: {ssid}")
    console.print(f"  Output: {output}")
    console.print()

    if dry_run:
        console.print("[yellow]Dry run - would perform the following:[/yellow]")
        console.print("  1. Build ISO via nix build")
        console.print(f"  2. Read WiFi password from {WIFI_PASSWORD_FILE}")
        console.print(f"  3. Inject WiFi credentials using scripts/inject-iso-wifi.sh")
        console.print(f"  4. Output to {output}")
        return

    # Build the ISO
    iso_file = build_iso(repo_root)

    # Read WiFi password
    password = read_wifi_password(repo_root)

    # Inject WiFi credentials
    inject_wifi(iso_file, ssid, password, output, repo_root)

    console.print()
    console.print("[bold green]Build complete![/bold green]")
