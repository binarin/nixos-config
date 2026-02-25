"""Main CLI entry point for ncf."""

import typer
from rich.console import Console

from .commands import (
    add_machine,
    build,
    ci,
    eval,
    init_machine,
    ipam_cmd,
    iso,
    list_machines,
    verify,
)

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

ci_app = typer.Typer(
    name="ci",
    help="Manage CI workflows",
    no_args_is_help=True,
)
app.add_typer(ci_app, name="ci")

iso_app = typer.Typer(
    name="iso",
    help="ISO image operations",
    no_args_is_help=True,
)
app.add_typer(iso_app, name="iso")

build_app = typer.Typer(
    name="build",
    help="Build NixOS configurations and related artifacts",
    no_args_is_help=True,
)
app.add_typer(build_app, name="build")

eval_app = typer.Typer(
    name="eval",
    help="Evaluate NixOS configurations (for debugging)",
    no_args_is_help=True,
)
app.add_typer(eval_app, name="eval")

machine_app = typer.Typer(
    name="machine",
    help="Manage NixOS machine configurations",
    no_args_is_help=True,
)
app.add_typer(machine_app, name="machine")

ipam_app = typer.Typer(
    name="ipam",
    help="IP address management operations",
    no_args_is_help=True,
)
app.add_typer(ipam_app, name="ipam")

console = Console()


# Common CLI options
def verbosity_callback(value: int) -> int:
    """Convert verbosity flags to level."""
    return value


def builder_callback(value: list[str]) -> list[str]:
    """Validate builder format."""
    return value


# Build commands
@build_app.command("nixos")
def build_nixos_cmd(
    configuration: str = typer.Argument(help="NixOS configuration name to build"),
    output: str = typer.Option(
        None, "--output", "-o", help="Output path for result symlink"
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    no_nom: bool = typer.Option(False, "--no-nom", help="Disable nix-output-monitor"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option("auto", "--jobs", "-j", help="Number of parallel jobs"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Build a NixOS configuration."""
    from pathlib import Path

    verbosity = 0 if quiet else (2 if verbose else 1)
    use_nom = None if not no_nom else False
    output_path = Path(output) if output else None

    build.run_nixos(
        configuration=configuration,
        output=output_path,
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
    )


@build_app.command("home")
def build_home_cmd(
    host: str = typer.Argument(help="Host name"),
    user: str = typer.Argument(help="User name"),
    output: str = typer.Option(
        None, "--output", "-o", help="Output path for result symlink"
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    no_nom: bool = typer.Option(False, "--no-nom", help="Disable nix-output-monitor"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option("auto", "--jobs", "-j", help="Number of parallel jobs"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Build a home-manager configuration."""
    from pathlib import Path

    verbosity = 0 if quiet else (2 if verbose else 1)
    use_nom = None if not no_nom else False
    output_path = Path(output) if output else None

    build.run_home(
        host=host,
        user=user,
        output=output_path,
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
    )


@build_app.command("lxc")
def build_lxc_cmd(
    target: str = typer.Argument(help="LXC target name"),
    output: str = typer.Option(
        None, "--output", "-o", help="Output path for result tarball"
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    no_nom: bool = typer.Option(False, "--no-nom", help="Disable nix-output-monitor"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option("auto", "--jobs", "-j", help="Number of parallel jobs"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
    inject_secrets: bool = typer.Option(
        False,
        "--inject-secrets",
        help="Inject decrypted secrets (SSH keys, age key) into the tarball",
    ),
):
    """Build an LXC tarball."""
    from pathlib import Path

    verbosity = 0 if quiet else (2 if verbose else 1)
    use_nom = None if not no_nom else False
    output_path = Path(output) if output else None

    build.run_lxc(
        target=target,
        output=output_path,
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
        inject_secrets=inject_secrets,
    )


@build_app.command("iso")
def build_iso_cmd(
    output: str = typer.Option(
        None, "--output", "-o", help="Output path for result symlink"
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    no_nom: bool = typer.Option(False, "--no-nom", help="Disable nix-output-monitor"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option("auto", "--jobs", "-j", help="Number of parallel jobs"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Build ISO image."""
    from pathlib import Path

    verbosity = 0 if quiet else (2 if verbose else 1)
    use_nom = None if not no_nom else False
    output_path = Path(output) if output else None

    build.run_iso(
        output=output_path,
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
    )


@build_app.command("all")
def build_all_cmd(
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    no_nom: bool = typer.Option(False, "--no-nom", help="Disable nix-output-monitor"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option(
        "auto", "--jobs", "-j", help="Number of parallel jobs for nix"
    ),
    max_parallel: int = typer.Option(
        None, "--max-parallel", "-P", help="Max parallel builds (default: CPU count)"
    ),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Build all NixOS configurations in parallel."""
    verbosity = 0 if quiet else (2 if verbose else 1)
    use_nom = None if not no_nom else False

    build.run_all(
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builder if builder else None,
        jobs=jobs,
        max_parallel=max_parallel,
        dry_run=dry_run,
    )


# Eval commands
@eval_app.command("nixos")
def eval_nixos_cmd(
    configuration: str = typer.Argument(help="NixOS configuration name to evaluate"),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Evaluate a NixOS configuration (get .drv path without building).

    Useful for debugging infinite recursion and evaluation errors.
    """
    verbosity = 0 if quiet else (2 if verbose else 1)
    eval.run_nixos(
        configuration=configuration,
        verbosity=verbosity,
        dry_run=dry_run,
    )


@eval_app.command("all")
def eval_all_cmd(
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    max_parallel: int = typer.Option(
        None,
        "--max-parallel",
        "-P",
        help="Max parallel evaluations (default: CPU count)",
    ),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Evaluate all NixOS configurations in parallel.

    Useful for debugging which configuration is causing evaluation errors.
    """
    verbosity = 0 if quiet else (2 if verbose else 1)
    eval.run_all(
        verbosity=verbosity,
        max_parallel=max_parallel,
        dry_run=dry_run,
    )


@eval_app.command("query")
def eval_query_cmd(
    configuration: str = typer.Argument(help="NixOS configuration name"),
    attribute: str = typer.Argument(help="Attribute path to evaluate"),
    json_output: bool = typer.Option(False, "--json", help="Output as JSON"),
    raw: bool = typer.Option(False, "--raw", help="Output raw string"),
    apply: str = typer.Option(None, "--apply", help="Apply a function to the result"),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Evaluate an arbitrary attribute from a NixOS configuration."""
    verbosity = 0 if quiet else (2 if verbose else 1)
    eval.run_query(
        configuration=configuration,
        attribute=attribute,
        json_output=json_output,
        raw=raw,
        apply=apply,
        verbosity=verbosity,
        dry_run=dry_run,
    )


@ci_app.command("generate")
def ci_generate_cmd(
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
):
    """Generate CI workflow YAML files.

    Reads ci.doBuild from each nixosConfiguration and generates
    workflow files that only build configurations with doBuild=true.
    """
    ci.run_generate(dry_run=dry_run)


@machine_app.command("add")
def machine_add_cmd(
    name: str = typer.Argument(help="Name for the new machine"),
    system: str = typer.Option(
        "x86_64-linux", "--system", "-s", help="System architecture"
    ),
    network: str = typer.Option(
        "home", "--network", "-n", help="Network for IP allocation"
    ),
    no_network: bool = typer.Option(False, "--no-network", help="Skip IP allocation"),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
):
    """Add a new NixOS machine configuration.

    Creates all necessary files for a new machine:
    - Host ID in inventory/host-id.toml
    - IP allocation in inventory/networks/<network>.toml
    - Machine directory with hardware-configuration.nix and disko.nix
    - Machine module in modules/machines/<name>.nix
    - Secrets via ncf secrets init-machine
    """
    actual_network = None if no_network else network
    add_machine.run(
        name=name,
        system=system,
        network=actual_network,
        dry_run=dry_run,
    )


@ipam_app.command("format")
def ipam_format_cmd(
    network: str = typer.Argument(
        None, help="Network to format (e.g., 'home'), or all if not specified"
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
):
    """Format network allocation files.

    Sorts IPs in ascending order, converts complex allocations to hash format,
    and adds unallocated IPs as comments for /24 networks.
    """
    ipam_cmd.run_format(network=network, dry_run=dry_run)


@iso_app.command("build-wifi")
def iso_build_wifi_cmd(
    output: str = typer.Option(
        None, "--output", "-o", help="Output path for the WiFi-enabled ISO"
    ),
    ssid: str = typer.Option(iso.DEFAULT_SSID, "--ssid", help="WiFi network name"),
    password: str = typer.Option(
        None,
        "--password",
        "-p",
        help="WiFi password (if not provided, reads from git-crypt file)",
    ),
    password_file: str = typer.Option(
        None,
        "--password-file",
        help="Path to file containing WiFi password",
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without executing"
    ),
):
    """Build ISO with WiFi credentials injected.

    Builds the NixOS ISO image and injects WiFi credentials into it.
    The WiFi password can be provided via --password, --password-file,
    or it will be read from the default git-crypt encrypted file.
    """
    from pathlib import Path

    output_path = Path(output) if output else None
    password_file_path = Path(password_file) if password_file else None
    iso.run_build_wifi(
        output=output_path,
        ssid=ssid,
        password=password,
        password_file=password_file_path,
        dry_run=dry_run,
    )


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
