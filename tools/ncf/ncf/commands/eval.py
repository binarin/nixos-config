"""Eval commands for ncf."""

import json
import os
import signal
import sys
import tempfile
import threading
import time
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


def run_nixos(
    configuration: str,
    verbosity: int = 1,
    dry_run: bool = False,
) -> str:
    """Evaluate a NixOS configuration and return the derivation path.

    Primary use case: Debugging infinite recursion and evaluation errors.

    Args:
        configuration: The NixOS configuration name to evaluate
        verbosity: 0=quiet, 1=normal, 2=verbose
        dry_run: Show what would be done without evaluating

    Returns:
        The derivation path (.drv)
    """
    repo_root = config.find_repo_root()
    flake_ref = f"{repo_root}#nixosConfigurations.{configuration}.config.system.build.toplevel.drvPath"

    if dry_run:
        console.print(f"[bold]Would evaluate:[/bold] {flake_ref}")
        return ""

    runner = NixRunner(
        verbosity=verbosity,
        repo_root=repo_root,
    )

    result = runner.run_eval(flake_ref, raw=True)
    drv_path = result.stdout.strip()

    if verbosity > 0:
        console.print(drv_path)

    return drv_path


def run_all(
    verbosity: int = 1,
    max_parallel: Optional[int] = None,
    dry_run: bool = False,
) -> None:
    """Evaluate all NixOS configurations in parallel.

    Primary use case: Debugging infinite recursion and evaluation errors.
    When `nix flake check` fails, it evaluates all outputs together, making
    it difficult to identify which specific configuration is causing the problem.
    This command evaluates each configuration individually in parallel, isolating
    failures and providing clear diagnostics about which configs have issues.

    Args:
        verbosity: 0=quiet, 1=normal, 2=verbose
        max_parallel: Max parallel evaluations (default: CPU count)
        dry_run: Show what would be done without evaluating
    """
    repo_root = config.find_repo_root()

    # Get all configurations
    console.print("Discovering NixOS configurations...")
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    configurations = get_nixos_configurations(runner)

    if not configurations:
        console.print(f"{YELLOW}Warning: No nixosConfigurations found{RESET}")
        return

    if max_parallel is None:
        max_parallel = os.cpu_count() or 4

    console.print(
        f"Found {len(configurations)} configurations to evaluate (parallelism: {max_parallel})"
    )
    console.print()

    if dry_run:
        console.print("[yellow]Dry run - would evaluate:[/yellow]")
        for cfg in configurations:
            console.print(f"  - {cfg}")
        return

    # Track results: config -> (success, time_seconds, error_msg)
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

    def eval_config(cfg: str) -> tuple[str, bool, float, str]:
        """Evaluate a single configuration."""
        if interrupted:
            return (cfg, False, 0.0, "Interrupted")

        cfg_start = time.time()
        flake_ref = f"{repo_root}#nixosConfigurations.{cfg}.config.system.build.toplevel.drvPath"

        eval_runner = NixRunner(
            verbosity=verbosity,
            repo_root=repo_root,
        )

        try:
            eval_runner.run_eval(flake_ref, raw=True)
            elapsed = time.time() - cfg_start
            return (cfg, True, elapsed, "")
        except Exception as e:
            elapsed = time.time() - cfg_start
            return (cfg, False, elapsed, str(e))

    try:
        with ThreadPoolExecutor(max_workers=max_parallel) as executor:
            futures = {executor.submit(eval_config, cfg): cfg for cfg in configurations}

            first_update = True
            for future in as_completed(futures):
                cfg, success, elapsed, error = future.result()
                with lock:
                    completed += 1
                    results[cfg] = (success, elapsed, error)

                    succeeded = sum(1 for s, _, _ in results.values() if s)
                    failed = sum(1 for s, _, _ in results.values() if not s)

                    # Progress line
                    progress = (
                        f"{BLUE}Progress:{RESET} {completed}/{len(configurations)} "
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
    print("━" * 40)

    succeeded = 0
    failed = 0
    failed_configs = []

    for i, cfg in enumerate(configurations):
        success, elapsed, error = results.get(cfg, (False, 0.0, "Not started"))
        status_str = f"[{i+1:2d}/{len(configurations):2d}] {cfg:30s}:"

        if success:
            print(f"{status_str} {GREEN}✓ OK    {RESET} ({int(elapsed):3d}s)")
            succeeded += 1
        else:
            print(f"{status_str} {RED}✗ FAILED{RESET} ({int(elapsed):3d}s)")
            failed += 1
            failed_configs.append(cfg)

    # Summary
    print()
    print("━" * 40)
    total_min = int(total_time) // 60
    total_sec = int(total_time) % 60

    if failed == 0:
        print(
            f"{BOLD}Summary:{RESET} {GREEN}{succeeded}/{len(configurations)} succeeded{RESET} "
            f"in {total_min}m{total_sec}s (parallel)"
        )
    else:
        print(
            f"{BOLD}Summary:{RESET} {GREEN}{succeeded}/{len(configurations)} succeeded{RESET}, "
            f"{RED}{failed}/{len(configurations)} failed{RESET} in {total_min}m{total_sec}s (parallel)"
        )
        print()
        print(f"{RED}Failed configurations:{RESET}")
        for cfg in failed_configs:
            print(f"  - {cfg}")
        print()
        print("To see detailed error for a specific configuration:")
        print(
            f"  nix eval '{repo_root}#nixosConfigurations.<config>.config.system.build.toplevel.drvPath'"
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

    Returns:
        The evaluation result
    """
    repo_root = config.find_repo_root()
    flake_ref = f"{repo_root}#nixosConfigurations.{configuration}.{attribute}"

    if dry_run:
        console.print(f"[bold]Would evaluate:[/bold] {flake_ref}")
        if apply:
            console.print(f"[bold]Apply:[/bold] {apply}")
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
    )
    output = result.stdout.strip()

    if verbosity > 0:
        print(output)

    return output
