"""Eval commands for ncf."""

import json
import os
import signal
import socket
import sys
import tempfile
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Optional

from .. import config
from ..external import ExternalToolError
from ..nix import NixRunner, get_nixos_configurations, get_home_configurations
from ..output import console


def get_default_parallelism() -> int:
    """Calculate default parallelism based on available memory.

    Returns (system memory in GB) / 6, with a minimum of 1.
    Special case: claude-nixos-config host is hardcoded to 32GB.
    """
    hostname = socket.gethostname()
    if hostname == "claude-nixos-config":
        memory_gb = 32
    else:
        try:
            with open("/proc/meminfo") as f:
                for line in f:
                    if line.startswith("MemTotal:"):
                        # MemTotal is in kB
                        mem_kb = int(line.split()[1])
                        memory_gb = mem_kb // (1024 * 1024)
                        break
                else:
                    memory_gb = 8  # fallback
        except (OSError, ValueError):
            memory_gb = 8  # fallback

    parallelism = max(1, memory_gb // 6)
    return parallelism


# ANSI colors for terminal output
GREEN = "\033[0;32m"
RED = "\033[0;31m"
YELLOW = "\033[0;33m"
BLUE = "\033[0;34m"
BOLD = "\033[1m"
RESET = "\033[0m"


def run_nixos(
    configuration: Optional[str] = None,
    verbosity: int = 1,
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> str:
    """Evaluate a NixOS configuration and return the derivation path.

    Primary use case: Debugging infinite recursion and evaluation errors.

    Args:
        configuration: The NixOS configuration name to evaluate.
                      If None, uses current machine's hostname.
        verbosity: 0=quiet, 1=normal, 2=verbose
        dry_run: Show what would be done without evaluating
        extra_nix_args: Extra arguments to pass to nix eval

    Returns:
        The derivation path (.drv)
    """
    repo_root = config.find_repo_root()

    # Default to current hostname if no configuration specified
    if configuration is None:
        configuration = socket.gethostname()
        if verbosity > 0:
            console.print(f"[dim]Using current machine: {configuration}[/dim]")

    # Validate that the configuration exists
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    configurations = get_nixos_configurations(runner)
    if configuration not in configurations:
        raise ExternalToolError(
            "nix",
            f"Configuration '{configuration}' not found. "
            f"Available configurations: {', '.join(sorted(configurations))}",
        )

    flake_ref = f"{repo_root}#nixosConfigurations.{configuration}.config.system.build.toplevel.drvPath"

    if dry_run:
        console.print(f"[bold]Would evaluate:[/bold] {flake_ref}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
        return ""

    runner = NixRunner(
        verbosity=verbosity,
        repo_root=repo_root,
    )

    result = runner.run_eval(flake_ref, raw=True, extra_args=extra_nix_args)
    drv_path = result.stdout.strip()

    if verbosity > 0:
        console.print(drv_path)

    return drv_path


def run_home(
    configuration: str,
    verbosity: int = 1,
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> str:
    """Evaluate a home-manager configuration and return the derivation path.

    Primary use case: Debugging infinite recursion and evaluation errors.

    Args:
        configuration: The home-manager configuration name to evaluate.
        verbosity: 0=quiet, 1=normal, 2=verbose
        dry_run: Show what would be done without evaluating
        extra_nix_args: Extra arguments to pass to nix eval

    Returns:
        The derivation path (.drv)
    """
    repo_root = config.find_repo_root()

    # Validate that the configuration exists
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    configurations = get_home_configurations(runner)
    if configuration not in configurations:
        raise ExternalToolError(
            "nix",
            f"Home configuration '{configuration}' not found. "
            f"Available configurations: {', '.join(sorted(configurations))}",
        )

    flake_ref = f"{repo_root}#homeConfigurations.{configuration}.activationPackage.drvPath"

    if dry_run:
        console.print(f"[bold]Would evaluate:[/bold] {flake_ref}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
        return ""

    runner = NixRunner(
        verbosity=verbosity,
        repo_root=repo_root,
    )

    result = runner.run_eval(flake_ref, raw=True, extra_args=extra_nix_args)
    drv_path = result.stdout.strip()

    if verbosity > 0:
        console.print(drv_path)

    return drv_path


def run_all(
    verbosity: int = 1,
    max_parallel: Optional[int] = None,
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> None:
    """Evaluate all NixOS and home-manager configurations in parallel.

    Primary use case: Debugging infinite recursion and evaluation
    errors.  When `nix flake check` fails, it evaluates all outputs
    together, making it difficult to identify which specific
    configuration is causing the problem. This command evaluates each
    configuration individually in parallel, isolating failures and
    providing clear diagnostics about which configs have issues. And
    also preventing excessive memory usage by `nix flake check`.

    Args:
        verbosity: 0=quiet, 1=normal, 2=verbose
        max_parallel: Max parallel evaluations (default: CPU count)
        dry_run: Show what would be done without evaluating
        extra_nix_args: Extra arguments to pass to nix eval

    """
    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=0, repo_root=repo_root)

    # Get all NixOS configurations
    console.print("Discovering NixOS configurations...")
    nixos_configs = get_nixos_configurations(runner)

    # Get all home-manager configurations
    console.print("Discovering home-manager configurations...")
    try:
        home_configs = get_home_configurations(runner)
    except Exception:
        home_configs = []
        console.print(f"{YELLOW}Warning: No homeConfigurations found{RESET}")

    if not nixos_configs and not home_configs:
        console.print(f"{YELLOW}Warning: No configurations found{RESET}")
        return

    # Build list of (display_name, flake_ref) tuples
    eval_targets: list[tuple[str, str]] = []

    for cfg in nixos_configs:
        flake_ref = f"{repo_root}#nixosConfigurations.{cfg}.config.system.build.toplevel.drvPath"
        eval_targets.append((f"nixos:{cfg}", flake_ref))

    for cfg in home_configs:
        flake_ref = f"{repo_root}#homeConfigurations.{cfg}.activationPackage.drvPath"
        eval_targets.append((f"home:{cfg}", flake_ref))

    if max_parallel is None:
        max_parallel = get_default_parallelism()

    console.print(
        f"Found {len(nixos_configs)} NixOS + {len(home_configs)} home-manager "
        f"= {len(eval_targets)} configurations to evaluate (parallelism: {max_parallel})"
    )
    console.print()

    if dry_run:
        console.print("[yellow]Dry run - would evaluate:[/yellow]")
        for name, _ in eval_targets:
            console.print(f"  - {name}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
        return

    # Track results: display_name -> (success, time_seconds, error_msg)
    results: dict[str, tuple[bool, float, str]] = {}
    completed = 0
    lock = threading.Lock()
    start_time = time.time()

    # Handle interrupts
    interrupted = False

    def signal_handler(signum, frame):
        nonlocal interrupted
        interrupted = True
        print(f"\n\n{YELLOW}Interrupted by user{RESET}")

    old_handler = signal.signal(signal.SIGINT, signal_handler)

    def eval_config(name: str, flake_ref: str) -> tuple[str, bool, float, str]:
        """Evaluate a single configuration."""
        if interrupted:
            return (name, False, 0.0, "Interrupted")

        cfg_start = time.time()

        eval_runner = NixRunner(
            verbosity=verbosity,
            repo_root=repo_root,
        )

        try:
            eval_runner.run_eval(flake_ref, raw=True, extra_args=extra_nix_args)
            elapsed = time.time() - cfg_start
            return (name, True, elapsed, "")
        except Exception as e:
            elapsed = time.time() - cfg_start
            return (name, False, elapsed, str(e))

    try:
        with ThreadPoolExecutor(max_workers=max_parallel) as executor:
            futures = {
                executor.submit(eval_config, name, flake_ref): name
                for name, flake_ref in eval_targets
            }

            first_update = True
            for future in as_completed(futures):
                name, success, elapsed, error = future.result()
                with lock:
                    completed += 1
                    results[name] = (success, elapsed, error)

                    succeeded = sum(1 for s, _, _ in results.values() if s)
                    failed = sum(1 for s, _, _ in results.values() if not s)

                    # Progress line
                    progress = (
                        f"{BLUE}Progress:{RESET} {completed}/{len(eval_targets)} "
                    )
                    progress += f"({GREEN}✓{succeeded}{RESET}"
                    if failed > 0:
                        progress += f" {RED}✗{failed}{RESET}"
                    progress += ")"

                    if first_update:
                        print(progress, end="    ", flush=True)
                        first_update = False
                    else:
                        print(f"\r{progress}    ", end="", flush=True)

                if interrupted:
                    break
    finally:
        signal.signal(signal.SIGINT, old_handler)

    print()  # Newline after progress

    total_time = time.time() - start_time

    # Display results
    print()
    print("Results:")
    print("━" * 50)

    succeeded = 0
    failed = 0
    failed_configs = []

    for i, (name, _) in enumerate(eval_targets):
        success, elapsed, error = results.get(name, (False, 0.0, "Not started"))
        status_str = f"[{i+1:2d}/{len(eval_targets):2d}] {name:40s}:"

        if success:
            print(f"{status_str} {GREEN}✓ OK    {RESET} ({int(elapsed):3d}s)")
            succeeded += 1
        else:
            print(f"{status_str} {RED}✗ FAILED{RESET} ({int(elapsed):3d}s)")
            failed += 1
            failed_configs.append(name)

    # Summary
    print()
    print("━" * 50)
    total_min = int(total_time) // 60
    total_sec = int(total_time) % 60

    if failed == 0:
        print(
            f"{BOLD}Summary:{RESET} {GREEN}{succeeded}/{len(eval_targets)} succeeded{RESET} "
            f"in {total_min}m{total_sec}s (parallel)"
        )
    else:
        print(
            f"{BOLD}Summary:{RESET} {GREEN}{succeeded}/{len(eval_targets)} succeeded{RESET}, "
            f"{RED}{failed}/{len(eval_targets)} failed{RESET} in {total_min}m{total_sec}s (parallel)"
        )
        print()
        print(f"{RED}Failed configurations:{RESET}")
        for name in failed_configs:
            print(f"  - {name}")
        print()
        print("To see detailed error for a specific configuration:")
        print(
            f"  nix eval '{repo_root}#nixosConfigurations.<config>.config.system.build.toplevel.drvPath'"
        )
        print(
            f"  nix eval '{repo_root}#homeConfigurations.<config>.activationPackage.drvPath'"
        )
        sys.exit(1)


def run_query(
    configuration: str,
    attribute: str,
    json_output: bool = False,
    raw: bool = False,
    apply: Optional[str] = None,
    verbosity: int = 1,
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> str:
    """Evaluate an arbitrary attribute from a NixOS configuration.

    Generic query command to replace ad-hoc nix eval calls.

    Args:
        configuration: The NixOS configuration name
        attribute: The attribute path to evaluate (e.g., "config.networking.hostName")
        json_output: Output as JSON
        raw: Output raw string
        apply: Apply a function to the result
        verbosity: 0=quiet, 1=normal, 2=verbose
        dry_run: Show what would be done without evaluating
        extra_nix_args: Extra arguments to pass to nix eval

    Returns:
        The evaluation result
    """
    repo_root = config.find_repo_root()
    flake_ref = f"{repo_root}#nixosConfigurations.{configuration}.{attribute}"

    if dry_run:
        console.print(f"[bold]Would evaluate:[/bold] {flake_ref}")
        if apply:
            console.print(f"[bold]Apply:[/bold] {apply}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
        return ""

    runner = NixRunner(
        verbosity=verbosity,
        repo_root=repo_root,
    )

    result = runner.run_eval(
        flake_ref,
        raw=raw,
        json_output=json_output,
        apply=apply,
        extra_args=extra_nix_args,
    )
    output = result.stdout.strip()

    if verbosity > 0:
        print(output)

    return output
