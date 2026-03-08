"""Main CLI entry point for ncf."""

import typer
from rich.console import Console
from typer._completion_classes import completion_init
from typer.completion import show_callback

# Initialize typer's completion classes with click early, before any shell
# completion handling. This is needed because we use add_completion=False and
# manually add --show-completion. Without this, when _NCF_COMPLETE is set,
# typer's shell_complete() would use click's completion classes instead of
# typer's, causing KeyError: 'COMP_WORDS' (click expects different env vars).
completion_init()

from .commands import (
    add_machine,
    aws_env,
    build,
    ci,
    deploy,
    eval,
    init_machine,
    ipam_cmd,
    iso,
    list_machines,
    provision_lxc,
    set_secret,
    verify,
)

app = typer.Typer(
    name="ncf",
    help="CLI tool for NixOS configuration management",
    no_args_is_help=True,
    add_completion=False,
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


@app.callback(invoke_without_command=True)
def app_callback(
    ctx: typer.Context,
    show_completion: bool = typer.Option(
        None,
        "--show-completion",
        callback=show_callback,
        expose_value=False,
        is_eager=True,
        help="Show completion for the current shell, to copy it or customize the installation.",
    ),
) -> None:
    """CLI tool for NixOS configuration management."""
    if ctx.invoked_subcommand is None:
        raise typer.Exit(0)


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
    fake_secrets: bool = typer.Option(
        False,
        "--fake-secrets",
        help="Use placeholder content instead of decrypting secrets (for testing)",
    ),
):
    """Build an LXC tarball."""
    from pathlib import Path

    verbosity = 0 if quiet else (2 if verbose else 1)
    use_nom = None if not no_nom else False
    output_path = Path(output) if output else None

    # --fake-secrets implies --inject-secrets
    if fake_secrets:
        inject_secrets = True

    build.run_lxc(
        target=target,
        output=output_path,
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
        inject_secrets=inject_secrets,
        fake_secrets=fake_secrets,
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
    configuration: str = typer.Argument(
        None, help="NixOS configuration name to evaluate (default: current hostname)"
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Evaluate a NixOS configuration (get .drv path without building).

    Useful for debugging infinite recursion and evaluation errors.
    If no configuration is specified, evaluates the current machine's configuration.
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


@ci_app.command("matrix")
def ci_matrix_cmd():
    """Output JSON array of configurations for CI dynamic matrix.

    Reads ci.doBuild from each nixosConfiguration and outputs
    a JSON array of configuration names that should be built.
    """
    ci.run_matrix()


@ci_app.command("fake-unlock")
def ci_fake_unlock_cmd():
    """Replace git-crypt encrypted files with placeholder content.

    Used for CI builds on fork PRs that don't have access to secrets.
    Replaces encrypted files with appropriate placeholders:
    - .jwe files: valid JWE format placeholder
    - .json files: empty JSON object
    - other files: simple text placeholder
    """
    ci.run_fake_unlock()


@ci_app.command("build-path")
def ci_build_path_cmd(
    entry: str = typer.Argument(
        help="Matrix entry (e.g., d:media, c:furfur, p:ncf, s:default)"
    ),
):
    """Output the nix build path for a matrix entry.

    Handles prefixed entries from 'ncf ci matrix':
    - d:<name> -> deploy-rs path
    - c:<name> -> nixosConfiguration toplevel
    - p:<name> -> package for x86_64-linux
    - s:<name> -> devShell for x86_64-linux
    """
    ci.run_build_path(entry)


@ci_app.command("expand-matrix-entry")
def ci_expand_matrix_entry_cmd(
    entry: str = typer.Argument(
        help="Matrix entry (e.g., d:media, c:furfur, p:ncf, s:default)"
    ),
):
    """Expand a matrix entry prefix to a human-readable name.

    Expands prefixed entries to descriptive names:
    - d:<name> -> deployable-<name>
    - c:<name> -> nixos-config-<name>
    - p:<name> -> package-<name>
    - s:<name> -> devshell-<name>
    """
    ci.run_expand_matrix_entry(entry)


@ci_app.command("external-deps")
def ci_external_deps_cmd():
    """Display all registered external tool dependencies.

    Shows which external tools ncf depends on, organized by tool and
    showing which modules use each tool and for what purpose.
    """
    ci.run_external_deps()


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
    machine_type: str = typer.Option(
        "default",
        "--type",
        "-t",
        help="Machine type: 'default' (with disko) or 'lxc' (Proxmox LXC container)",
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
    force: bool = typer.Option(
        False,
        "--force",
        "-f",
        help="Proceed even if working directory has uncommitted changes",
    ),
):
    """Add a new NixOS machine configuration.

    Creates all necessary files for a new machine:
    - Host ID in inventory/host-id.toml
    - IP allocation in inventory/networks/<network>.toml (with MAC for LXC)
    - Machine directory with hardware-configuration.nix and disko.nix
    - Machine module in modules/machines/<name>.nix
    - Secrets via ncf secrets init-machine

    For LXC machines (--type lxc), generates a MAC address and configures
    the machine to import the lxc module instead of disko.

    If any step fails, all changes are rolled back automatically.
    Use --force to proceed despite uncommitted changes (rollback may be incomplete).
    """
    actual_network = None if no_network else network
    add_machine.run(
        name=name,
        system=system,
        network=actual_network,
        machine_type=machine_type,
        dry_run=dry_run,
        force=force,
    )


@machine_app.command("provision")
def machine_provision_cmd(
    machine: str = typer.Argument(help="Machine name to provision"),
    proxmox_host: str = typer.Option(
        ..., "--proxmox-host", "-p", help="Proxmox host to provision on"
    ),
    bridge: str = typer.Option("vmbr0", "--bridge", "-b", help="Network bridge name"),
    reuse_remote_tarball: bool = typer.Option(
        False,
        "--reuse-remote-tarball",
        help="Skip tarball build/copy if remote tarball exists",
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
):
    """Provision a Proxmox LXC container.

    Builds an LXC tarball with secrets, copies it to the Proxmox host,
    and creates the container with the configuration from NixOS.

    If the container already exists, validates the configuration and
    reports any mismatches without modifying the container.
    """
    provision_lxc.run(
        machine=machine,
        proxmox_host=proxmox_host,
        bridge=bridge,
        reuse_remote_tarball=reuse_remote_tarball,
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
        ..., "--output", "-o", help="Output path for the WiFi-enabled ISO"
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

    output_path = Path(output)
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


@secrets_app.command("set")
def set_secret_cmd(
    sops_file: str = typer.Argument(help="Path to the sops-encrypted YAML file"),
    key_path: str = typer.Argument(
        help="YAML path to the secret (e.g., 'service/password' or 'db.password')"
    ),
    length: int = typer.Option(24, "--length", "-l", help="Password length"),
    mode: str = typer.Option(
        "SNCL",
        "--mode",
        "-M",
        help="apg mode: N=Numeric, C=Capital, L=Lowercase, S=Special",
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Show what would be done without making changes"
    ),
):
    """Generate a random password and set it in a sops-encrypted file.

    Uses apg to generate secure passwords and sops --set to update the file.
    The generated password is never printed to stdout/stderr for security.

    Examples:
        ncf secrets set secrets/media/secrets.yaml jellyfin/api-key
        ncf secrets set -l 32 secrets/forgejo/secrets.yaml runner-token
        ncf secrets set -M NCL secrets/db/secrets.yaml postgres.password
    """
    from pathlib import Path

    set_secret.run(
        sops_file=Path(sops_file),
        key_path=key_path,
        length=length,
        mode=mode,
        dry_run=dry_run,
    )


@secrets_app.command("aws-env")
def aws_env_cmd(
    sops_file: str = typer.Option(
        None,
        "--file",
        "-f",
        help="Path to sops-encrypted YAML file (default: secrets/<hostname>/user-binarin.yaml)",
    ),
):
    """Print Garage S3 credentials as AWS environment variable exports.

    Outputs export statements for AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
    from the garage section of the sops-encrypted secrets file.

    Usage:
        eval $(ncf secrets aws-env)
        eval $(ncf secrets aws-env -f secrets/other/user-binarin.yaml)
    """
    from pathlib import Path

    sops_path = Path(sops_file) if sops_file else None
    aws_env.run(sops_file=sops_path)


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


# Deploy commands
@app.command("deploy")
def deploy_cmd(
    target: str = typer.Argument(help="Deploy target name"),
    profile: str = typer.Option("system", "--profile", "-p", help="Profile to deploy"),
    boot: bool = typer.Option(False, "--boot", help="Deploy to boot loader and reboot"),
    no_rollback: bool = typer.Option(
        False, "--no-rollback", help="Disable auto/magic rollback"
    ),
    skip_unchanged: bool = typer.Option(
        True,
        "--skip-if-unchanged/--no-skip-if-unchanged",
        help="Skip if remote system already matches target",
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option("auto", "--jobs", "-j", help="Number of parallel jobs"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Deploy a single machine.

    Deploys a NixOS configuration to a remote machine using deploy-rs.
    By default, skips deployment if the remote system is already up to date.

    Note: --boot and --no-rollback are mutually exclusive.

    Examples:
        ncf deploy forgejo
        ncf deploy media --boot
        ncf deploy monitor --no-rollback
    """
    import sys

    # Mutual exclusion check
    if boot and no_rollback:
        console.print(
            "[red]Error: --boot and --no-rollback are mutually exclusive[/red]"
        )
        console.print("  --boot: deploys to boot loader, then reboots")
        console.print("  --no-rollback: disables rollback for live switch")
        sys.exit(1)

    verbosity = 0 if quiet else (2 if verbose else 1)

    success = deploy.run_single(
        target=target,
        profile=profile,
        boot=boot,
        no_rollback=no_rollback,
        skip_if_unchanged=skip_unchanged,
        verbosity=verbosity,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
    )

    if not success:
        sys.exit(1)


@app.command("deploy-all")
def deploy_all_cmd(
    boot: bool = typer.Option(
        False, "--boot", help="Deploy to boot loader and reboot each machine"
    ),
    no_rollback: bool = typer.Option(
        False, "--no-rollback", help="Disable auto/magic rollback"
    ),
    skip_unchanged: bool = typer.Option(
        True,
        "--skip-if-unchanged/--no-skip-if-unchanged",
        help="Skip machines that are already up to date",
    ),
    stop_on_failure: bool = typer.Option(
        True,
        "--stop-on-failure/--no-stop-on-failure",
        help="Stop on first failure",
    ),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress output"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output"),
    builder: list[str] = typer.Option(
        [], "--builder", "-b", help="Remote builder (repeatable)"
    ),
    jobs: str = typer.Option("auto", "--jobs", "-j", help="Number of parallel jobs"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Show what would be done"),
):
    """Deploy all deployable machines sequentially.

    Deploys all machines that have deploy-rs nodes configured.
    By default, stops on first failure and skips up-to-date machines.

    Note: --boot and --no-rollback are mutually exclusive.

    Examples:
        ncf deploy-all
        ncf deploy-all --boot
        ncf deploy-all --no-rollback --no-stop-on-failure
    """
    import sys

    # Mutual exclusion check
    if boot and no_rollback:
        console.print(
            "[red]Error: --boot and --no-rollback are mutually exclusive[/red]"
        )
        console.print("  --boot: deploys to boot loader, then reboots")
        console.print("  --no-rollback: disables rollback for live switch")
        sys.exit(1)

    verbosity = 0 if quiet else (2 if verbose else 1)

    success = deploy.run_all(
        boot=boot,
        no_rollback=no_rollback,
        skip_if_unchanged=skip_unchanged,
        stop_on_failure=stop_on_failure,
        verbosity=verbosity,
        builders=builder if builder else None,
        jobs=jobs,
        dry_run=dry_run,
    )

    if not success:
        sys.exit(1)


def main():
    """Entry point for the CLI."""
    app()


if __name__ == "__main__":
    main()
