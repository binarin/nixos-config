"""Deploy commands for ncf."""

import os
import subprocess
import time
from pathlib import Path
from typing import Optional

from .. import config
from ..external import ExternalToolError, run_command
from ..nix import NixRunner
from ..output import console
from . import ci


def check_git_crypt_unlocked() -> bool:
    """Check if the repository is unlocked with git-crypt.

    Returns True if unlocked, False if locked.
    """
    repo_root = config.find_repo_root()
    result = subprocess.run(
        ["git", "config", "--local", "--get", "filter.git-crypt.smudge"],
        cwd=repo_root,
        capture_output=True,
        text=True,
    )
    return result.returncode == 0 and bool(result.stdout.strip())


def get_deploy_hostname(target: str) -> str:
    """Get the hostname for a deploy target from the flake."""
    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    result = runner.run_eval(
        f".#deploy.nodes.{target}.hostname",
        raw=True,
    )
    return result.stdout.strip()


def get_remote_current_system(hostname: str) -> Optional[str]:
    """Get the current system path from a remote host.

    Returns the store path or None if SSH fails.
    """
    try:
        result = subprocess.run(
            ["ssh", f"root@{hostname}", "readlink", "/run/current-system"],
            capture_output=True,
            text=True,
            timeout=30,
        )
        if result.returncode == 0:
            return result.stdout.strip()
        return None
    except (subprocess.TimeoutExpired, Exception):
        return None


def get_local_system_path(target: str, profile: str = "system") -> str:
    """Get the underlying NixOS system store path for a deploy target.

    Builds the deploy-rs profile (an activatable wrapper) and resolves the
    underlying nixos-system path by reading the 'activate' symlink.
    """
    repo_root = config.find_repo_root()

    # Build the deploy profile and get the store path directly
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    flake_ref = f"{repo_root}#deploy.nodes.{target}.profiles.{profile}.path"
    result = runner.run_build(flake_ref, output=None, print_out_paths=True)

    activatable_path = result.stdout.strip()

    # The activatable wrapper contains symlinks into the real nixos-system path.
    # Read the 'activate' symlink to extract the underlying system store path.
    activate_link = os.path.join(activatable_path, "activate")
    if os.path.islink(activate_link):
        # e.g. /nix/store/...-nixos-system-.../activate -> extract the directory
        return os.path.dirname(os.readlink(activate_link))

    return activatable_path


def _ssh_no_multiplex(
    hostname: str, *args: str, timeout: int = 30
) -> subprocess.CompletedProcess:
    """Run an SSH command bypassing connection multiplexing."""
    cmd = [
        "ssh",
        "-o",
        "ControlPath=none",
        "-o",
        "ConnectTimeout=5",
        "-o",
        "BatchMode=yes",
        f"root@{hostname}",
        *args,
    ]
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


def get_remote_boot_id(hostname: str) -> Optional[str]:
    """Get the boot_id from a remote host.

    Returns boot_id string or None if failed.
    """
    try:
        result = _ssh_no_multiplex(hostname, "cat", "/proc/sys/kernel/random/boot_id")
        if result.returncode == 0:
            return result.stdout.strip()
        return None
    except (subprocess.TimeoutExpired, Exception):
        return None


def get_remote_uptime(hostname: str) -> Optional[float]:
    """Get the uptime in seconds from a remote host.

    Returns uptime in seconds or None if failed.
    """
    try:
        result = _ssh_no_multiplex(hostname, "cat", "/proc/uptime")
        if result.returncode == 0:
            # /proc/uptime format: "uptime_seconds idle_seconds"
            return float(result.stdout.strip().split()[0])
        return None
    except (subprocess.TimeoutExpired, Exception):
        return None


def wait_for_reboot(
    hostname: str, old_boot_id: Optional[str], timeout: int = 300
) -> bool:
    """Wait for a machine to reboot by polling for a new boot_id.

    Uses boot_id comparison which is race-free — works even if the
    machine reboots faster than our polling interval.

    Returns True if reboot was successful, False otherwise.
    """
    console.print(f"  Waiting for {hostname} to reboot...")
    start_time = time.time()
    while time.time() - start_time < timeout:
        new_boot_id = get_remote_boot_id(hostname)
        if new_boot_id is not None:
            if old_boot_id is None or new_boot_id != old_boot_id:
                uptime = get_remote_uptime(hostname)
                uptime_str = f", uptime: {uptime:.0f}s" if uptime is not None else ""
                console.print(
                    f"  [green]Reboot complete[/green] (new boot_id{uptime_str})"
                )
                return True
        time.sleep(2)

    console.print(f"[red]  Error: {hostname} did not reboot within {timeout}s[/red]")
    return False


def run_deploy_command(
    target: str,
    profile: str = "system",
    boot: bool = False,
    no_rollback: bool = False,
    verbosity: int = 1,
    ssh_opts: str = "",
    extra_nix_args: Optional[list[str]] = None,
) -> bool:
    """Run the deploy-rs deploy command.

    Returns True on success, False on failure.
    """
    repo_root = config.find_repo_root()

    cmd = [
        "deploy",
        f"{repo_root}#{target}.{profile}",
        "-s",  # Skip flake checks
    ]

    if boot:
        cmd.append("--boot")

    if no_rollback:
        cmd.extend(["--auto-rollback", "false", "--magic-rollback", "false"])

    if ssh_opts:
        cmd.extend(["--ssh-opts", ssh_opts])

    # Add verbosity and extra nix args after --
    nix_args = []
    if verbosity >= 2:
        nix_args.append("-v")
    elif verbosity == 0:
        nix_args.append("--quiet")

    if extra_nix_args:
        nix_args.extend(extra_nix_args)

    if nix_args:
        cmd.append("--")
        cmd.extend(nix_args)

    try:
        subprocess.run(cmd, cwd=repo_root, check=True)
        return True
    except subprocess.CalledProcessError:
        return False


def run_single(
    target: str,
    profile: str = "system",
    boot: bool = False,
    no_rollback: bool = False,
    skip_if_unchanged: bool = True,
    verbosity: int = 1,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> bool:
    """Deploy a single machine.

    Returns True on success, False on failure.
    """
    # Check git-crypt
    if not check_git_crypt_unlocked():
        console.print("[red]ERROR: Repository is locked with git-crypt.[/red]")
        console.print("Please unlock it first with: git-crypt unlock")
        return False

    # Get hostname
    try:
        hostname = get_deploy_hostname(target)
    except Exception as e:
        console.print(f"[red]ERROR: Could not get hostname for {target}: {e}[/red]")
        return False

    console.print(f"[bold]Deploying {target}[/bold] ({hostname})")

    # Skip if unchanged check
    if skip_if_unchanged:
        console.print("  Checking if deployment is needed...")
        remote_path = get_remote_current_system(hostname)
        if remote_path:
            if dry_run:
                console.print(f"  Remote system: {remote_path}")
                console.print("  (dry-run: would build to check if changed)")
            else:
                try:
                    local_path = get_local_system_path(target, profile)
                    console.print(f"  Proposed: {local_path}")
                    if local_path == remote_path:
                        console.print(f"  [green]Skipping: already up to date[/green]")
                        return True
                    console.print(f"  Remote:   {remote_path}")
                except Exception as e:
                    console.print(
                        f"[yellow]  Warning: Could not determine local path: {e}[/yellow]"
                    )
        else:
            console.print(
                "[yellow]  Warning: Could not get remote system path[/yellow]"
            )

    if dry_run:
        console.print(f"  [yellow]Would deploy {target}[/yellow]")
        if boot:
            console.print("  [yellow]Would reboot after deployment[/yellow]")
        if extra_nix_args:
            console.print(
                f"  [yellow]Extra nix args: {' '.join(extra_nix_args)}[/yellow]"
            )
        return True

    # Run deploy
    mode = "boot" if boot else "switch"
    console.print(f"  Deploying ({mode} mode)...")

    success = run_deploy_command(
        target=target,
        profile=profile,
        boot=boot,
        no_rollback=no_rollback,
        verbosity=verbosity,
        extra_nix_args=extra_nix_args,
    )

    if not success:
        console.print(f"[red]  FAILED: Deploy to {target} failed[/red]")
        return False

    # Handle boot mode reboot
    if boot:
        # Record boot_id before triggering reboot for race-free detection
        old_boot_id = get_remote_boot_id(hostname)
        console.print("  Triggering reboot...")
        try:
            subprocess.run(
                [
                    "ssh",
                    "-o",
                    "ControlPath=none",
                    f"root@{hostname}",
                    "systemctl",
                    "reboot",
                ],
                check=False,
                timeout=30,
            )
        except subprocess.TimeoutExpired:
            pass  # Expected — SSH may hang as machine goes down
        except Exception as e:
            console.print(f"[yellow]  Warning: Could not trigger reboot: {e}[/yellow]")

        if not wait_for_reboot(hostname, old_boot_id):
            console.print(f"[red]  FAILED: Reboot of {target} failed[/red]")
            return False

    console.print(f"[green]  SUCCESS: {target} deployed[/green]")
    return True


def run_all(
    boot: bool = False,
    no_rollback: bool = False,
    skip_if_unchanged: bool = True,
    stop_on_failure: bool = True,
    verbosity: int = 1,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> bool:
    """Deploy all deployable machines.

    Returns True if all succeeded, False if any failed.
    """
    # Check git-crypt
    if not check_git_crypt_unlocked():
        console.print("[red]ERROR: Repository is locked with git-crypt.[/red]")
        console.print("Please unlock it first with: git-crypt unlock")
        return False

    # Get all deploy nodes
    deploy_nodes = ci.get_deploy_nodes()

    if not deploy_nodes:
        console.print("[yellow]No deploy nodes found[/yellow]")
        return True

    console.print(f"[bold]Deploying {len(deploy_nodes)} machines[/bold]")
    for node in deploy_nodes:
        console.print(f"  - {node}")

    # Results tracking
    results: dict[str, bool] = {}
    succeeded = 0
    failed = 0

    for node in deploy_nodes:
        console.print()
        success = run_single(
            target=node,
            boot=boot,
            no_rollback=no_rollback,
            skip_if_unchanged=skip_if_unchanged,
            verbosity=verbosity,
            builders=builders,
            jobs=jobs,
            dry_run=dry_run,
            extra_nix_args=extra_nix_args,
        )
        results[node] = success

        if success:
            succeeded += 1
        else:
            failed += 1
            if stop_on_failure and not no_rollback:
                console.print(f"\n[red]Stopping due to failure on {node}[/red]")
                break

    # Summary
    console.print()
    console.print("[bold]" + "=" * 40 + "[/bold]")
    console.print(f"[bold]Summary:[/bold] {succeeded} succeeded, {failed} failed")

    if failed > 0:
        console.print("\n[red]Failed machines:[/red]")
        for node, success in results.items():
            if not success:
                console.print(f"  - {node}")

    return failed == 0
