"""Main CLI entry point for ncf."""

import typer
from rich.console import Console

from .commands import init_machine, list_machines, verify

app = typer.Typer(
    name="ncf",
    help="CLI tool for NixOS configuration management",
    no_args_is_help=True,
)

secrets_app = typer.Typer(
    name="secrets",
    help="Manage NixOS secrets",
    no_args_is_help=True,
)
app.add_typer(secrets_app, name="secrets")

console = Console()


@secrets_app.command("init-machine")
def init_machine_cmd(
    name: str = typer.Argument(help="Machine name"),
    no_user_key: bool = typer.Option(
        False, "--no-user-key", help="Skip user-binarin-age generation"
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
):
    """Initialize secrets for a new machine.

    Creates SSH host keys, user age key, and updates .sops.yaml.
    Handles existing secrets gracefully (for migration).
    """
    init_machine.run(name, no_user_key=no_user_key, dry_run=dry_run)


@secrets_app.command("list")
def list_cmd(
    json_output: bool = typer.Option(False, "--json", help="Output as JSON"),
):
    """List all machines and their secrets status."""
    list_machines.run(json_output=json_output)


@secrets_app.command("verify")
def verify_cmd(
    machine: str = typer.Argument(None, help="Machine name (or all if not specified)"),
    all_machines: bool = typer.Option(False, "--all", "-a", help="Check all machines"),
):
    """Verify secrets integrity for a machine or all machines."""
    verify.run(machine=machine, all_machines=all_machines)


@secrets_app.command("show-keys")
def show_keys_cmd(
    machine: str = typer.Argument(help="Machine name"),
):
    """Show derived age keys for a machine."""
    from . import config
    from .external import (
        ssh_to_age,
        age_keygen_extract_public,
        sops_decrypt_to_stdout,
        is_sops_encrypted,
    )
    import tempfile
    from pathlib import Path

    machine_dir = config.get_machine_secrets_dir(machine)
    if not machine_dir.exists():
        console.print(f"[red]Machine '{machine}' not found in secrets/[/red]")
        raise typer.Exit(1)

    console.print(f"[bold]Keys for machine: {machine}[/bold]\n")

    # Server age key from SSH ed25519 public key
    ed25519_pub = config.ssh_host_key_path(machine_dir, "ed25519", public=True)
    if ed25519_pub.exists():
        try:
            server_age = ssh_to_age(ed25519_pub)
            console.print(f"[green]Server age key:[/green] {server_age}")
            console.print(f"  (derived from {ed25519_pub.name})")
        except Exception as e:
            console.print(f"[red]Could not derive server age key: {e}[/red]")
    else:
        console.print("[yellow]No SSH ed25519 public key found[/yellow]")

    # User age key
    age_key = config.user_age_key_path(machine_dir)
    age_pub = config.user_age_key_path(machine_dir, public=True)

    if age_pub.exists():
        pub_key = age_pub.read_text().strip()
        console.print(f"[green]User age key:[/green] {pub_key}")
        console.print(f"  (from {age_pub.name})")
    elif age_key.exists():
        if is_sops_encrypted(age_key):
            # Need to decrypt to extract public key
            try:
                decrypted = sops_decrypt_to_stdout(age_key)
                # Write to temp file and extract public key
                with tempfile.NamedTemporaryFile(
                    mode="w", suffix=".key", delete=False
                ) as f:
                    f.write(decrypted)
                    temp_path = Path(f.name)
                try:
                    pub_key = age_keygen_extract_public(temp_path)
                    console.print(f"[green]User age key:[/green] {pub_key}")
                    console.print(f"  (extracted from encrypted {age_key.name})")
                finally:
                    temp_path.unlink()
            except Exception as e:
                console.print(f"[red]Could not extract user age key: {e}[/red]")
        else:
            try:
                pub_key = age_keygen_extract_public(age_key)
                console.print(f"[green]User age key:[/green] {pub_key}")
                console.print(f"  (extracted from {age_key.name})")
            except Exception as e:
                console.print(f"[red]Could not extract user age key: {e}[/red]")
    else:
        console.print("[yellow]No user age key found[/yellow]")


def main():
    """Entry point for the CLI."""
    app()


if __name__ == "__main__":
    main()
