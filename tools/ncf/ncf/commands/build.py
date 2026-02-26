"""Build commands for ncf."""

import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Optional

from rich.console import Console

from .. import config
from ..external import ExternalToolError
from ..nix import NixRunner, get_nixos_configurations
from ..secrets_inject import (
    gather_secrets_for_machine,
    decrypt_secrets_to_tempdir,
    generate_fake_secrets_to_tempdir,
)

console = Console()

# ANSI colors for terminal output
GREEN = "\033[0;32m"
RED = "\033[0;31m"
YELLOW = "\033[0;33m"
BLUE = "\033[0;34m"
BOLD = "\033[1m"
RESET = "\033[0m"


def get_cache_dir() -> Path:
    """Get the cache directory for build outputs."""
    cache_dir = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
    return cache_dir / "nixos-config"


def run_nixos(
    configuration: str,
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
) -> None:
    """Build a NixOS configuration.

    Args:
        configuration: The NixOS configuration name to build
        output: Output path for the result symlink (default: ~/.cache/nixos-config/nixos-configuration/<config>)
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
    """
    repo_root = config.find_repo_root()

    if output is None:
        output = get_cache_dir() / "nixos-configuration" / configuration

    flake_ref = (
        f"{repo_root}#nixosConfigurations.{configuration}.config.system.build.toplevel"
    )

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        console.print(f"[bold]Output:[/bold] {output}")
        if builders:
            console.print(f"[bold]Builders:[/bold] {', '.join(builders)}")
        return

    console.print(f"[bold]Building NixOS configuration:[/bold] {configuration}")

    runner = NixRunner(
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builders or [],
        keep_going=True,
        jobs=jobs,
        repo_root=repo_root,
    )

    runner.run_build(flake_ref, output=output)
    console.print(f"[green]✓[/green] Built {configuration} -> {output}")


def run_home(
    host: str,
    user: str,
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
) -> None:
    """Build a home-manager configuration.

    Args:
        host: The host name
        user: The user name
        output: Output path for the result symlink
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
    """
    repo_root = config.find_repo_root()

    if output is None:
        output = get_cache_dir() / "home-configuration" / host / user

    flake_ref = f"{repo_root}#nixosConfigurations.{host}.config.home-manager.users.{user}.home.activationPackage"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        console.print(f"[bold]Output:[/bold] {output}")
        return

    console.print(f"[bold]Building home-manager configuration:[/bold] {host}/{user}")

    runner = NixRunner(
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builders or [],
        keep_going=True,
        jobs=jobs,
        repo_root=repo_root,
    )

    runner.run_build(flake_ref, output=output)
    console.print(f"[green]✓[/green] Built {host}/{user} -> {output}")


def run_lxc(
    target: str,
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    inject_secrets: bool = False,
    fake_secrets: bool = False,
) -> None:
    """Build an LXC tarball.

    Args:
        target: The LXC target name
        output: Output path for the result symlink
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
        inject_secrets: Inject decrypted secrets into the tarball
        fake_secrets: Use placeholder content instead of decrypting secrets
    """
    repo_root = config.find_repo_root()

    if output is None:
        output = repo_root / f"proxmox-lxc-{target}.tar.xz"

    flake_ref = f"{repo_root}#nixosConfigurations.{target}.config.system.build.tarball"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        console.print(f"[bold]Output:[/bold] {output}")
        if inject_secrets:
            if fake_secrets:
                console.print("[bold]Would inject fake secrets (placeholders)[/bold]")
            else:
                console.print("[bold]Would inject secrets[/bold]")
        return

    console.print(f"[bold]Building LXC tarball:[/bold] {target}")

    runner = NixRunner(
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builders or [],
        keep_going=True,
        jobs=jobs,
        repo_root=repo_root,
    )

    if inject_secrets:
        # Build to a temporary symlink first
        with tempfile.TemporaryDirectory(prefix="ncf-lxc-build-") as temp_build_dir:
            temp_output = Path(temp_build_dir) / "result"
            runner.run_build(flake_ref, output=temp_output)

            # Find the actual tarball in the nix store result
            tarball_path = _find_tarball_in_result(temp_output)

            if fake_secrets:
                console.print("[bold]Injecting fake secrets into tarball...[/bold]")
            else:
                console.print("[bold]Injecting secrets into tarball...[/bold]")
            _inject_secrets_into_tarball(
                target, tarball_path, output, runner, fake_secrets
            )

        suffix = " (fake)" if fake_secrets else ""
        console.print(
            f"[green]✓[/green] Built {target} with secrets{suffix} -> {output}"
        )
    else:
        runner.run_build(flake_ref, output=output)
        console.print(f"[green]✓[/green] Built {target} -> {output}")


def _find_tarball_in_result(result_path: Path) -> Path:
    """Find the .tar.xz tarball in a nix build result.

    The nix build result for system.build.tarball is a directory
    containing the tarball, possibly in a nested subdirectory.
    """
    if result_path.is_symlink():
        result_path = result_path.resolve()

    # Look for .tar.xz file recursively in the result directory
    if result_path.is_dir():
        tarballs = list(result_path.glob("**/*.tar.xz"))
        if tarballs:
            return tarballs[0]

    raise ExternalToolError(
        "tarball", f"Could not find .tar.xz tarball in {result_path}"
    )


def _inject_secrets_into_tarball(
    machine_name: str,
    source_tarball: Path,
    output_path: Path,
    runner: NixRunner,
    fake_secrets: bool = False,
) -> None:
    """Inject decrypted secrets into a tarball.

    This function:
    1. Gathers secrets configuration from NixOS config
    2. Decrypts secrets (or generates fakes) to a temp directory
    3. Copies the tarball (decompressed)
    4. Appends secrets files to the tarball
    5. Re-compresses the tarball

    Args:
        machine_name: The NixOS configuration name
        source_tarball: Path to the source .tar.xz tarball
        output_path: Path for the output tarball with secrets
        runner: NixRunner instance for nix eval
        fake_secrets: Use placeholder content instead of decrypting
    """
    # Gather secrets
    secrets = gather_secrets_for_machine(machine_name, runner)
    if not secrets:
        console.print("[yellow]Warning: No secrets found to inject[/yellow]")
        # Still copy the tarball to output
        shutil.copy2(source_tarball, output_path)
        return

    console.print(f"  Found {len(secrets)} secret(s) to inject")

    # Create secrets in temp directory (decrypt or generate fakes)
    if fake_secrets:
        secrets_dir = generate_fake_secrets_to_tempdir(secrets)
    else:
        secrets_dir = decrypt_secrets_to_tempdir(secrets)

    try:
        # Create a temp file for the decompressed tarball
        with tempfile.NamedTemporaryFile(
            prefix="ncf-lxc-", suffix=".tar", delete=False
        ) as temp_tar:
            temp_tar_path = Path(temp_tar.name)

        try:
            # Decompress the source tarball
            console.print("  Decompressing tarball...")
            subprocess.run(
                ["xz", "-d", "-c", str(source_tarball)],
                stdout=open(temp_tar_path, "wb"),
                check=True,
            )

            # Append secrets to the tarball
            console.print("  Appending secrets...")
            subprocess.run(
                [
                    "tar",
                    "--append",
                    "-f",
                    str(temp_tar_path),
                    "-C",
                    str(secrets_dir),
                    ".",
                ],
                check=True,
            )

            # Re-compress the tarball (using all CPU threads for parallel compression)
            console.print("  Compressing tarball...")
            subprocess.run(
                ["xz", "-T0", "-c", str(temp_tar_path)],
                stdout=open(output_path, "wb"),
                check=True,
            )

        finally:
            # Clean up temp tar file
            if temp_tar_path.exists():
                temp_tar_path.unlink()

    finally:
        # Clean up secrets directory
        shutil.rmtree(secrets_dir, ignore_errors=True)


def run_iso(
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
) -> None:
    """Build ISO image.

    Args:
        output: Output path for the result symlink
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
    """
    repo_root = config.find_repo_root()

    if output is None:
        output = get_cache_dir() / "nixos-configuration" / "iso"

    flake_ref = f"{repo_root}#nixosConfigurations.iso.config.system.build.isoImage"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        console.print(f"[bold]Output:[/bold] {output}")
        return

    console.print("[bold]Building ISO image[/bold]")

    runner = NixRunner(
        verbosity=verbosity,
        use_nom=use_nom,
        builders=builders or [],
        keep_going=True,
        jobs=jobs,
        repo_root=repo_root,
    )

    runner.run_build(flake_ref, output=output)
    console.print(f"[green]✓[/green] Built ISO -> {output}")


def run_all(
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    max_parallel: Optional[int] = None,
    dry_run: bool = False,
) -> None:
    """Build all NixOS configurations.

    Args:
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs for nix
        max_parallel: Max parallel builds (default: CPU count)
        dry_run: Show what would be done without building
    """
    repo_root = config.find_repo_root()
    cache_dir = get_cache_dir() / "nixos-configuration"

    # Get all configurations
    console.print("[bold]Discovering NixOS configurations...[/bold]")
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    configurations = get_nixos_configurations(runner)

    if not configurations:
        console.print("[yellow]No nixosConfigurations found[/yellow]")
        return

    console.print(f"Found {len(configurations)} configurations to build")

    if dry_run:
        console.print("\n[yellow]Dry run - would build:[/yellow]")
        for cfg in configurations:
            console.print(f"  - {cfg}")
        return

    if max_parallel is None:
        max_parallel = os.cpu_count() or 4

    # Track results
    results: dict[str, tuple[bool, str]] = {}
    completed = 0
    lock = threading.Lock()

    # Handle interrupts
    interrupted = False

    def signal_handler(signum, frame):
        nonlocal interrupted
        interrupted = True
        console.print("\n[yellow]Interrupted, waiting for running builds...[/yellow]")

    old_handler = signal.signal(signal.SIGINT, signal_handler)

    def build_config(cfg: str) -> tuple[str, bool, str]:
        """Build a single configuration."""
        if interrupted:
            return (cfg, False, "Interrupted")

        output = cache_dir / cfg
        flake_ref = (
            f"{repo_root}#nixosConfigurations.{cfg}.config.system.build.toplevel"
        )

        build_runner = NixRunner(
            verbosity=0,  # Quiet for parallel builds
            use_nom=False,  # No nom for parallel builds
            builders=builders or [],
            keep_going=True,
            jobs=jobs,
            repo_root=repo_root,
        )

        try:
            build_runner.run_build(flake_ref, output=output)
            return (cfg, True, "")
        except Exception as e:
            return (cfg, False, str(e))

    try:
        with ThreadPoolExecutor(max_workers=max_parallel) as executor:
            futures = {
                executor.submit(build_config, cfg): cfg for cfg in configurations
            }

            for future in as_completed(futures):
                cfg, success, error = future.result()
                with lock:
                    completed += 1
                    results[cfg] = (success, error)

                    succeeded = sum(1 for s, _ in results.values() if s)
                    failed = sum(1 for s, _ in results.values() if not s)

                    # Progress line
                    progress = (
                        f"{BLUE}Progress:{RESET} {completed}/{len(configurations)} "
                    )
                    progress += f"({GREEN}✓{succeeded}{RESET}"
                    if failed > 0:
                        progress += f" {RED}✗{failed}{RESET}"
                    progress += ")"
                    print(f"\r{progress}    ", end="", flush=True)

                if interrupted:
                    break
    finally:
        signal.signal(signal.SIGINT, old_handler)

    print()  # Newline after progress

    # Display results
    console.print("\n[bold]Results:[/bold]")
    console.print("━" * 40)

    succeeded = 0
    failed = 0
    failed_configs = []

    for i, cfg in enumerate(configurations):
        success, error = results.get(cfg, (False, "Not started"))
        status_str = f"[{i+1:2d}/{len(configurations):2d}] {cfg}:"

        if success:
            console.print(f"{status_str} [green]✓ OK[/green]")
            succeeded += 1
        else:
            console.print(f"{status_str} [red]✗ FAILED[/red]")
            failed += 1
            failed_configs.append(cfg)

    # Summary
    console.print()
    console.print("━" * 40)
    if failed == 0:
        console.print(
            f"[bold]Summary:[/bold] [green]{succeeded}/{len(configurations)} succeeded[/green]"
        )
    else:
        console.print(
            f"[bold]Summary:[/bold] [green]{succeeded}/{len(configurations)} succeeded[/green], "
            f"[red]{failed}/{len(configurations)} failed[/red]"
        )
        console.print()
        console.print("[red]Failed configurations:[/red]")
        for cfg in failed_configs:
            console.print(f"  - {cfg}")
        console.print()
        console.print("To see detailed error for a specific configuration:")
        console.print(f"  ncf build nixos <config>")
        sys.exit(1)
