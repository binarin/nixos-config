"""List all machines and their secrets status."""

import json

from rich.console import Console
from rich.table import Table

from .. import config
from ..models import MachineSecrets
from ..sops_yaml import SopsYaml

console = Console()


def run(json_output: bool = False) -> None:
    """List all machines and their secrets status."""
    repo_root = config.find_repo_root()
    secrets_dir = config.get_secrets_dir(repo_root)

    if not secrets_dir.exists():
        console.print("[red]No secrets directory found[/red]")
        return

    # Load .sops.yaml to check which machines have keys configured
    sops = SopsYaml(config.get_sops_yaml_path(repo_root))
    sops.load()

    machines = []
    for machine_path in sorted(secrets_dir.iterdir()):
        if not machine_path.is_dir():
            continue

        machine = MachineSecrets.from_directory(machine_path)
        machine.in_sops_yaml = sops.has_machine_keys(machine.name)
        machines.append(machine)

    if json_output:
        output = []
        for m in machines:
            output.append(
                {
                    "name": m.name,
                    "has_ssh_keys": m.has_all_ssh_keys,
                    "ssh_keys_encrypted": m.all_ssh_keys_encrypted,
                    "has_user_key": m.has_user_key,
                    "user_key_encrypted": m.user_key_encrypted,
                    "has_secrets_yaml": m.secrets_yaml_exists,
                    "has_user_yaml": m.user_binarin_yaml_exists,
                    "in_sops_yaml": m.in_sops_yaml,
                    "is_complete": m.is_complete,
                }
            )
        console.print(json.dumps(output, indent=2))
        return

    # Create a nice table
    table = Table(title="NixOS Machine Secrets")
    table.add_column("Machine", style="cyan")
    table.add_column("SSH Keys", justify="center")
    table.add_column("User Key", justify="center")
    table.add_column("Secrets", justify="center")
    table.add_column("SOPS", justify="center")
    table.add_column("Status", justify="center")

    for m in machines:
        # SSH keys status
        if m.has_all_ssh_keys:
            if m.all_ssh_keys_encrypted:
                ssh_status = "[green]OK[/green]"
            else:
                ssh_status = "[yellow]UNENC[/yellow]"
        else:
            ssh_status = "[red]MISSING[/red]"

        # User key status
        if m.has_user_key:
            if m.user_key_encrypted:
                user_status = "[green]OK[/green]"
            else:
                user_status = "[yellow]UNENC[/yellow]"
        else:
            user_status = "[dim]-[/dim]"

        # Secrets files status
        secrets_parts = []
        if m.secrets_yaml_exists:
            secrets_parts.append("S")
        if m.user_binarin_yaml_exists:
            secrets_parts.append("U")
        secrets_status = (
            "[green]" + "/".join(secrets_parts) + "[/green]"
            if secrets_parts
            else "[dim]-[/dim]"
        )

        # SOPS config status
        sops_status = "[green]OK[/green]" if m.in_sops_yaml else "[red]NO[/red]"

        # Overall status
        if m.is_complete:
            overall = "[bold green]READY[/bold green]"
        elif m.has_all_ssh_keys or m.has_user_key:
            overall = "[yellow]PARTIAL[/yellow]"
        else:
            overall = "[dim]EMPTY[/dim]"

        table.add_row(
            m.name, ssh_status, user_status, secrets_status, sops_status, overall
        )

    console.print(table)

    # Summary
    complete = sum(1 for m in machines if m.is_complete)
    partial = sum(
        1
        for m in machines
        if (m.has_all_ssh_keys or m.has_user_key) and not m.is_complete
    )
    empty = len(machines) - complete - partial

    console.print(
        f"\n[bold]Summary:[/bold] {complete} ready, {partial} partial, {empty} empty"
    )
    console.print(
        "\n[dim]Legend: SSH=SSH host keys, User=user age key, Secrets=S(secrets.yaml)/U(user-binarin.yaml), SOPS=in .sops.yaml[/dim]"
    )
