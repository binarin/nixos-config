"""Add a new NixOS machine configuration."""

import secrets
import subprocess
import json
from pathlib import Path

import git
import tomlkit
from jinja2 import Environment, PackageLoader
from rich.console import Console
from rich.panel import Panel

from .. import config as ncf_config
from .. import external
from .. import ipam as ipam_utils

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


def generate_mac_from_host_id(host_id: str) -> str:
    """Generate a locally administered MAC address from host ID.

    Uses the host ID (4 bytes / 8 hex chars) as the last 4 octets of the MAC.
    The first byte is 02 (locally administered, unicast).
    Format: 02:00:XX:XX:XX:XX where XX:XX:XX:XX is the host ID.
    """
    # Split host ID into 4 pairs
    octets = [host_id[i : i + 2] for i in range(0, 8, 2)]
    return f"02:00:{octets[0]}:{octets[1]}:{octets[2]}:{octets[3]}".upper()


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
    machine_type: str = "default",
    dry_run: bool = False,
) -> None:
    """Add a new NixOS machine configuration.

    Steps:
    1. Validate machine name doesn't exist
    2. Generate host ID
    3. Update inventory/host-id.toml
    4. Allocate IP in network (if network specified, with MAC for LXC)
    5. Create machine directory
    6. Create empty hardware-configuration.nix and disko.nix
    7. Get current stateVersion from nixpkgs
    8. Create machine module from template
    9. Run ncf secrets init-machine
    10. Stage all new files in git
    11. Run nix fmt
    """
    type_label = f" (type: {machine_type})" if machine_type != "default" else ""
    console.print(Panel(f"Adding new machine: [bold]{name}[/bold]{type_label}"))

    repo_root = ncf_config.find_repo_root()

    if dry_run:
        console.print("[yellow]DRY RUN - no changes will be made[/yellow]\n")

    # Step 1: Validate machine name
    console.print("\n[bold]Step 1:[/bold] Validating machine name")

    host_id_path = repo_root / "inventory" / "host-id.toml"
    host_ids = tomlkit.parse(host_id_path.read_text())

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
        host_id_path.write_text(tomlkit.dumps(host_ids))
        console.print(f'  [green]Added {name} = "{host_id}"[/green]')

    # Step 4: Allocate IP in network
    allocated_ip = None
    allocated_mac = None
    if network:
        console.print(f"\n[bold]Step 4:[/bold] Allocating IP in network '{network}'")
        network_path = repo_root / "inventory" / "networks" / f"{network}.toml"

        if not network_path.exists():
            console.print(f"  [red]Network file not found: {network_path}[/red]")
            raise SystemExit(1)

        network_doc = ipam_utils.load_network_file(network_path)
        network_info = ipam_utils.get_network_info(network_doc)
        network_prefix = network_info["network"]
        ipam_data = dict(network_doc.get("ipam", {}))

        allocated_ip = ipam_utils.find_next_available_ip(ipam_data, network_prefix)
        if not allocated_ip:
            console.print(f"  [red]No available IPs in network '{network}'[/red]")
            raise SystemExit(1)

        # Generate MAC for LXC machines
        if machine_type == "lxc":
            allocated_mac = generate_mac_from_host_id(host_id)
            console.print(f"  [green]Generated MAC: {allocated_mac}[/green]")

        if dry_run:
            mac_info = f" (MAC: {allocated_mac})" if allocated_mac else ""
            console.print(
                f'  [yellow]Would allocate: {allocated_ip} = "{name}"{mac_info}[/yellow]'
            )
        else:
            formatted_doc = ipam_utils.add_allocation(
                network_doc, allocated_ip, name, mac=allocated_mac
            )
            ipam_utils.save_network_file(network_path, formatted_doc)
            mac_info = f" (MAC: {allocated_mac})" if allocated_mac else ""
            console.print(
                f'  [green]Allocated {allocated_ip} = "{name}"{mac_info}[/green]'
            )
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

    # Configure template based on machine type
    extra_imports: list[str] = []
    extra_config = ""

    if machine_type == "lxc":
        extra_imports = [
            "self.nixosModules.lxc",
            "self.nixosModules.baseline",
        ]
        extra_config = ""  # proxmoxLXC options have sensible defaults
    else:
        extra_imports = [
            "self.nixosModules.disko",
            "self.nixosModules.baseline",
        ]

    module_content = template.render(
        machine_name=name,
        system=system,
        state_version=state_version,
        has_network=network is not None,
        network=network or "home",
        extra_imports=extra_imports,
        extra_config=extra_config,
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

    # Step 11: Run nix fmt
    console.print("\n[bold]Step 11:[/bold] Running nix fmt")
    if dry_run:
        console.print("  [yellow]Would run: nix fmt[/yellow]")
    else:
        external.nix_fmt(repo_root)
        console.print("  [green]Formatted files with nix fmt[/green]")
        # Re-stage files to include any formatting changes
        try:
            repo.index.add(files_to_add)
            console.print("  [green]Re-staged formatted files[/green]")
        except Exception as e:
            console.print(f"  [yellow]Warning: Could not re-stage files: {e}[/yellow]")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nCreated machine '{name}' with:")
        console.print(f"  - Host ID: {host_id}")
        if allocated_ip:
            console.print(f"  - IP address: {allocated_ip} (network: {network})")
        console.print(f"  - State version: {state_version}")
        console.print(f"\nNext steps:")
        console.print(f"  1. Review staged changes: git status")
        if machine_type == "lxc":
            console.print(f"  2. Edit proxmoxLXC settings: modules/machines/{name}.nix")
            console.print(f"  3. Build tarball: ncf build lxc {name}")
            console.print(
                f"  4. Provision: ncf machine provision {name} --proxmox-host <HOST>"
            )
        else:
            console.print(
                f"  2. Edit hardware config: machines/{name}/hardware-configuration.nix"
            )
            console.print(f"  3. Edit disk layout: machines/{name}/disko.nix")
            console.print(f"  4. Build to test: ncf build system {name}")
