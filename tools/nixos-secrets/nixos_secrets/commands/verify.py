"""Verify secrets integrity for machines."""

from pathlib import Path

from rich.console import Console
from rich.panel import Panel

from .. import config
from .. import external
from ..models import MachineSecrets
from ..sops_yaml import SopsYaml

console = Console()


def verify_machine(machine: MachineSecrets, sops: SopsYaml) -> list[str]:
    """Verify a single machine's secrets.

    Returns a list of issues found.
    """
    issues = []
    machine_dir = machine.directory

    # Check SSH keys
    for key_type in config.SSH_KEY_TYPES:
        key_path = config.ssh_host_key_path(machine_dir, key_type)
        pub_path = config.ssh_host_key_path(machine_dir, key_type, public=True)

        if not key_path.exists() and not pub_path.exists():
            issues.append(f"Missing SSH {key_type} key pair")
        elif key_path.exists() and not external.is_sops_encrypted(key_path):
            issues.append(f"SSH {key_type} private key is not encrypted")
        elif not pub_path.exists():
            issues.append(f"SSH {key_type} public key missing (private exists)")

    # Check user age key
    age_key_path = config.user_age_key_path(machine_dir)
    if age_key_path.exists() and not external.is_sops_encrypted(age_key_path):
        issues.append("User age private key is not encrypted")

    # Check secrets files
    secrets_yaml = config.secrets_yaml_path(machine_dir)
    user_yaml = config.user_binarin_yaml_path(machine_dir)

    if not secrets_yaml.exists():
        issues.append("Missing secrets.yaml")
    if not user_yaml.exists():
        issues.append("Missing user-binarin.yaml")

    # Check .sops.yaml configuration
    if not sops.has_machine_keys(machine.name):
        issues.append("Machine keys not configured in .sops.yaml")

    # Verify SSH key can be converted to age key
    ed25519_pub = config.ssh_host_key_path(machine_dir, "ed25519", public=True)
    if ed25519_pub.exists():
        try:
            external.ssh_to_age(ed25519_pub)
        except Exception as e:
            issues.append(f"Cannot derive age key from SSH ed25519: {e}")

    # Try to decrypt a secret to verify keys work
    if secrets_yaml.exists() and external.is_sops_encrypted(secrets_yaml):
        try:
            external.sops_decrypt_to_stdout(secrets_yaml)
        except Exception as e:
            issues.append(f"Cannot decrypt secrets.yaml: {e}")

    return issues


def run(machine: str = None, all_machines: bool = False) -> None:
    """Verify secrets integrity."""
    repo_root = config.find_repo_root()
    secrets_dir = config.get_secrets_dir(repo_root)

    if not secrets_dir.exists():
        console.print("[red]No secrets directory found[/red]")
        return

    # Check external tools
    try:
        external.ensure_tools_available()
    except external.ExternalToolError as e:
        console.print(f"[red]{e}[/red]")
        return

    # Load .sops.yaml
    sops = SopsYaml(config.get_sops_yaml_path(repo_root))
    sops.load()

    # Determine which machines to verify
    if machine:
        machine_dir = secrets_dir / machine
        if not machine_dir.exists():
            console.print(f"[red]Machine '{machine}' not found in secrets/[/red]")
            return
        machines_to_check = [MachineSecrets.from_directory(machine_dir)]
    elif all_machines:
        machines_to_check = [
            MachineSecrets.from_directory(p)
            for p in sorted(secrets_dir.iterdir())
            if p.is_dir()
        ]
    else:
        console.print("[yellow]Specify a machine name or use --all to verify all machines[/yellow]")
        return

    total_issues = 0
    for m in machines_to_check:
        console.print(Panel(f"Verifying: [bold]{m.name}[/bold]"))

        issues = verify_machine(m, sops)

        if issues:
            for issue in issues:
                console.print(f"  [red]\u2717[/red] {issue}")
            total_issues += len(issues)
        else:
            console.print("  [green]\u2713 All checks passed[/green]")

        console.print()

    # Summary
    if total_issues:
        console.print(f"[bold red]Found {total_issues} issue(s)[/bold red]")
    else:
        console.print(f"[bold green]All {len(machines_to_check)} machine(s) verified successfully[/bold green]")
