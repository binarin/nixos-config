"""Build commands for ncf."""

import json
import os
import signal
import sys
import tempfile
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Optional

from rich.console import Console

from .. import config
from ..nix import NixRunner, get_nixos_configurations

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
    """
    repo_root = config.find_repo_root()

    if output is None:
        output = repo_root / f"proxmox-lxc-{target}"

    flake_ref = f"{repo_root}#nixosConfigurations.{target}.config.system.build.tarball"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        console.print(f"[bold]Output:[/bold] {output}")
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

    runner.run_build(flake_ref, output=output)
    console.print(f"[green]✓[/green] Built {target} -> {output}")


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
