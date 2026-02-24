"""Add a new NixOS machine configuration."""

import secrets
import subprocess
import json
from pathlib import Path

import git
import toml
from jinja2 import Environment, PackageLoader
from rich.console import Console
from rich.panel import Panel

from .. import config as ncf_config

console = Console()


def get_nixpkgs_rev(repo_root: Path) -> str:
    """Get the nixpkgs revision from flake.lock."""
    result = subprocess.run(
        ["nix", "flake", "metadata", "--json", str(repo_root)],
        capture_output=True,
        text=True,
        check=True,
    )
    metadata = json.loads(result.stdout)
    return metadata["locks"]["nodes"]["nixpkgs"]["locked"]["rev"]


def get_state_version(nixpkgs_rev: str) -> str:
    """Get the NixOS state version for a given nixpkgs revision."""
    result = subprocess.run(
        [
            "nix",
            "eval",
            f"github:nixos/nixpkgs/{nixpkgs_rev}#lib.trivial.release",
            "--raw",
        ],
        capture_output=True,
        text=True,
        check=True,
    )
    return result.stdout.strip()


def generate_host_id() -> str:
    """Generate a random 8-character hex host ID."""
    return secrets.token_hex(4)


def load_toml(path: Path) -> dict:
    """Load a TOML file."""
    return toml.load(path)


def save_toml(path: Path, data: dict) -> None:
    """Save data to a TOML file."""
    with open(path, "w") as f:
        toml.dump(data, f)


def find_next_available_ip(ipam: dict, network_prefix: str) -> str | None:
    """Find the first available IP in the network.

    Scans from .2 to .254 (skipping .1 which is typically gateway).
    """
    # Extract the base network (e.g., "192.168.2" from "192.168.2.0")
    base = ".".join(network_prefix.split(".")[:-1])

    used_ips = set(ipam.keys())

    for i in range(2, 255):
        ip = f"{base}.{i}"
        if ip not in used_ips:
            return ip

    return None


def run(
    name: str,
    system: str = "x86_64-linux",
    network: str | None = "home",
    dry_run: bool = False,
) -> None:
    """Add a new NixOS machine configuration.

    Steps:
    1. Validate machine name doesn't exist
    2. Generate host ID
    3. Update inventory/host-id.toml
    4. Allocate IP in network (if network specified)
    5. Create machine directory
    6. Create empty hardware-configuration.nix and disko.nix
    7. Get current stateVersion from nixpkgs
    8. Create machine module from template
    9. Run ncf secrets init-machine
    10. Stage all new files in git
    """
    console.print(Panel(f"Adding new machine: [bold]{name}[/bold]"))

    repo_root = ncf_config.find_repo_root()

    if dry_run:
        console.print("[yellow]DRY RUN - no changes will be made[/yellow]\n")

    # Step 1: Validate machine name
    console.print("\n[bold]Step 1:[/bold] Validating machine name")

    host_id_path = repo_root / "inventory" / "host-id.toml"
    host_ids = load_toml(host_id_path)

    if name in host_ids:
        console.print(f"  [red]Machine '{name}' already exists in host-id.toml[/red]")
        raise SystemExit(1)

    machine_module = repo_root / "modules" / "machines" / f"{name}.nix"
    if machine_module.exists():
        console.print(
            f"  [red]Machine module already exists: {machine_module.relative_to(repo_root)}[/red]"
        )
        raise SystemExit(1)

    machine_dir = repo_root / "machines" / name
    if machine_dir.exists():
        console.print(
            f"  [red]Machine directory already exists: {machine_dir.relative_to(repo_root)}[/red]"
        )
        raise SystemExit(1)

    console.print("  [green]Machine name is available[/green]")

    # Step 2: Generate host ID
    console.print("\n[bold]Step 2:[/bold] Generating host ID")
    host_id = generate_host_id()
    console.print(f"  [green]Generated host ID: {host_id}[/green]")

    # Step 3: Update host-id.toml
    console.print("\n[bold]Step 3:[/bold] Updating inventory/host-id.toml")
    if dry_run:
        console.print(f'  [yellow]Would add: {name} = "{host_id}"[/yellow]')
    else:
        host_ids[name] = host_id
        save_toml(host_id_path, host_ids)
        console.print(f'  [green]Added {name} = "{host_id}"[/green]')

    # Step 4: Allocate IP in network
    allocated_ip = None
    if network:
        console.print(f"\n[bold]Step 4:[/bold] Allocating IP in network '{network}'")
        network_path = repo_root / "inventory" / "networks" / f"{network}.toml"

        if not network_path.exists():
            console.print(f"  [red]Network file not found: {network_path}[/red]")
            raise SystemExit(1)

        network_data = load_toml(network_path)
        network_prefix = network_data["info"]["network"]
        ipam = network_data.get("ipam", {})

        allocated_ip = find_next_available_ip(ipam, network_prefix)
        if not allocated_ip:
            console.print(f"  [red]No available IPs in network '{network}'[/red]")
            raise SystemExit(1)

        if dry_run:
            console.print(
                f'  [yellow]Would allocate: {allocated_ip} = "{name}"[/yellow]'
            )
        else:
            ipam[allocated_ip] = name
            network_data["ipam"] = ipam
            save_toml(network_path, network_data)
            console.print(f'  [green]Allocated {allocated_ip} = "{name}"[/green]')
    else:
        console.print(
            "\n[bold]Step 4:[/bold] [dim]Skipping IP allocation (--no-network)[/dim]"
        )

    # Step 5: Create machine directory
    console.print(f"\n[bold]Step 5:[/bold] Creating machine directory")
    if dry_run:
        console.print(f"  [yellow]Would create: machines/{name}/[/yellow]")
    else:
        machine_dir.mkdir(parents=True)
        console.print(f"  [green]Created machines/{name}/[/green]")

    # Step 6: Create empty files
    console.print("\n[bold]Step 6:[/bold] Creating empty Nix files")
    hw_config = machine_dir / "hardware-configuration.nix"
    disko_config = machine_dir / "disko.nix"

    for nix_file in [hw_config, disko_config]:
        if dry_run:
            console.print(
                f"  [yellow]Would create: {nix_file.relative_to(repo_root)}[/yellow]"
            )
        else:
            nix_file.write_text("{}\n")
            console.print(f"  [green]Created {nix_file.relative_to(repo_root)}[/green]")

    # Step 7: Get stateVersion
    console.print("\n[bold]Step 7:[/bold] Getting current NixOS state version")
    if dry_run:
        state_version = "XX.XX"
        console.print("  [yellow]Would determine state version from nixpkgs[/yellow]")
    else:
        nixpkgs_rev = get_nixpkgs_rev(repo_root)
        console.print(f"  [dim]nixpkgs revision: {nixpkgs_rev}[/dim]")
        state_version = get_state_version(nixpkgs_rev)
        console.print(f"  [green]State version: {state_version}[/green]")

    # Step 8: Create machine module from template
    console.print("\n[bold]Step 8:[/bold] Creating machine module from template")

    env = Environment(loader=PackageLoader("ncf", "templates"))
    template = env.get_template("machine.nix.j2")

    module_content = template.render(
        machine_name=name,
        system=system,
        state_version=state_version,
        has_network=network is not None,
        network=network or "home",
    )

    if dry_run:
        console.print(f"  [yellow]Would create: modules/machines/{name}.nix[/yellow]")
    else:
        machine_module.write_text(module_content)
        console.print(f"  [green]Created modules/machines/{name}.nix[/green]")

    # Step 9: Run secrets init-machine
    console.print("\n[bold]Step 9:[/bold] Initializing machine secrets")
    if dry_run:
        console.print("  [yellow]Would run: ncf secrets init-machine[/yellow]")
    else:
        from . import init_machine

        init_machine.run(name, dry_run=False)

    # Step 10: Stage all new files in git
    console.print("\n[bold]Step 10:[/bold] Staging files in git")
    if dry_run:
        console.print("  [yellow]Would stage all new files[/yellow]")
    else:
        try:
            repo = git.Repo(repo_root)
            files_to_add = [
                str(host_id_path.relative_to(repo_root)),
                str(machine_dir.relative_to(repo_root)),
                str(machine_module.relative_to(repo_root)),
            ]
            if network:
                network_path = repo_root / "inventory" / "networks" / f"{network}.toml"
                files_to_add.append(str(network_path.relative_to(repo_root)))

            repo.index.add(files_to_add)
            console.print("  [green]Staged files in git[/green]")
        except Exception as e:
            console.print(f"  [yellow]Warning: Could not stage files: {e}[/yellow]")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nCreated machine '{name}' with:")
        console.print(f"  - Host ID: {host_id}")
        if allocated_ip:
            console.print(f"  - IP address: {allocated_ip} (network: {network})")
        console.print(f"  - State version: {state_version}")
        console.print(f"\nNext steps:")
        console.print(f"  1. Review staged changes: git status")
        console.print(
            f"  2. Edit hardware config: machines/{name}/hardware-configuration.nix"
        )
        console.print(f"  3. Edit disk layout: machines/{name}/disko.nix")
        console.print(
            f"  4. Build to test: nix build .#nixosConfigurations.{name}.config.system.build.toplevel"
        )
