"""Deploy commands for ncf."""

import json
import os
import re
import subprocess
import time
from concurrent.futures import ThreadPoolExecutor
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


def get_deploy_ssh_user(target: str) -> str:
    """Get the SSH user for a deploy target from the flake.

    Returns the sshUser attribute, falling back to "root" if not set.
    """
    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=0, repo_root=repo_root)
    try:
        result = runner.run_eval(
            f".#deploy.nodes.{target}.sshUser",
            raw=True,
        )
        return result.stdout.strip()
    except Exception:
        return "root"


def _ssh_readlink_f(
    hostname: str, user: str, path: str, timeout: int = 30
) -> Optional[str]:
    """Read a fully-resolved symlink on a remote host via SSH.

    Uses readlink -f to canonicalize every symlink in the chain,
    including relative ones like ``home-manager-138-link``.
    Returns the final target or None if SSH fails.
    """
    try:
        result = subprocess.run(
            [
                "ssh",
                "-o",
                "ConnectTimeout=5",
                "-o",
                "BatchMode=yes",
                f"{user}@{hostname}",
                "readlink",
                "-f",
                path,
            ],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        if result.returncode == 0:
            return result.stdout.strip()
        return None
    except (subprocess.TimeoutExpired, Exception):
        return None


def get_remote_current_system(hostname: str) -> Optional[str]:
    """Get the running NixOS system path.

    For NixOS machines /run/current-system is a direct symlink; plain
    ``readlink`` is sufficient (and avoids resolving through bootloader
    indirections on special setups).
    """
    return _ssh_readlink_f(hostname, "root", "/run/current-system")


def get_remote_system_manager_profile(
    hostname: str,
    ssh_user: str,
) -> Optional[str]:
    """Get the active system-manager config derivation path.

    system-manager keeps its own profile at
    ``/nix/var/nix/profiles/system-manager-profiles/system-manager``
    which tracks the underlying ``-system-manager`` derivation directly,
    without the deploy-rs activatable wrapper.
    """
    return _ssh_readlink_f(
        hostname,
        ssh_user,
        "/nix/var/nix/profiles/system-manager-profiles/system-manager",
    )


def get_remote_home_manager_profile(
    hostname: str,
    ssh_user: str,
) -> Optional[str]:
    """Get the current home-manager generation path."""
    return _ssh_readlink_f(hostname, ssh_user, ".local/state/nix/profiles/home-manager")


def _derivation_lookup(drv_info: dict, drv_path: str) -> dict:
    """Look up a derivation entry in 'nix derivation show' output.

    'nix derivation show' uses basenames (without /nix/store/) as keys.
    """
    derivations = drv_info.get("derivations", {})
    # Try exact match first, then basename match
    if drv_path in derivations:
        return derivations[drv_path]
    basename = os.path.basename(drv_path)
    if basename in derivations:
        return derivations[basename]
    raise KeyError(f"Derivation {drv_path} not found in show output")


def _resolve_drv_output_path(drv_path: str) -> str:
    """Resolve a derivation's output path via 'nix derivation show'.

    Takes a .drv path and returns the output store path without building.
    """
    repo_root = config.find_repo_root()
    result = subprocess.run(
        ["nix", "derivation", "show", drv_path],
        capture_output=True,
        text=True,
        cwd=repo_root,
    )
    drv_info = json.loads(result.stdout)
    out_path = _derivation_lookup(drv_info, drv_path)["outputs"]["out"]["path"]
    # 'nix derivation show' stores paths without /nix/store/ prefix
    if not out_path.startswith("/"):
        out_path = os.path.join("/nix/store", out_path)
    return out_path


def _classify_config_type(path: str) -> str:
    """Deduce config type from an underlying derivation path.

    Returns ``"nixos-system"``, ``"system-manager"``, or ``"home-manager"``.
    """
    basename = os.path.basename(path)
    if "-nixos-system-" in basename:
        return "nixos-system"
    if "-system-manager" in basename and "-system-manager-" not in basename:
        return "system-manager"
    return "home-manager"


def get_deploy_toplevel_path(target: str, profile: str = "system") -> str:
    """Get the underlying config store path for a deploy profile without building.

    Inspects the deploy-rs wrapper derivation's ``structuredAttrs.chosenOutputs``
    to find the wrapped config (nixos-system, system-manager, or home-manager
    generation). Falls back to the wrapper's own output path.
    """
    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=0, repo_root=repo_root)

    drv_ref = f"{repo_root}#deploy.nodes.{target}.profiles.{profile}.path.drvPath"
    result = runner.run_eval(drv_ref, raw=True)
    profile_drv = result.stdout.strip()

    show_result = subprocess.run(
        ["nix", "derivation", "show", profile_drv],
        capture_output=True,
        text=True,
        cwd=repo_root,
    )
    drv_info = json.loads(show_result.stdout)
    entry = _derivation_lookup(drv_info, profile_drv)

    chosen = entry.get("structuredAttrs", {}).get("chosenOutputs", [])
    if chosen and chosen[0].get("paths"):
        return chosen[0]["paths"][0]

    return _resolve_drv_output_path(profile_drv)


def _collect_all_remote_paths(
    hostname: str,
    ssh_user: str,
) -> dict[str, Optional[str]]:
    """Collect all possible remote comparison paths for a host in one SSH session.

    Because the config type (nixos-system / system-manager / home-manager) is
    not known until the batch eval completes, we fetch all candidates in
    parallel so that when the batch data arrives the right path is already
    available.
    """
    return {
        "nixos-system": get_remote_current_system(hostname),
        "system-manager": get_remote_system_manager_profile(hostname, ssh_user),
        "home-manager": get_remote_home_manager_profile(hostname, ssh_user),
    }


def _parse_nixos_release(path: str) -> Optional[str]:
    """Extract NixOS release version from a store path.

    NixOS system paths embed the release as ``-YY.MM.DATE.REV``.
    Example: ``...-nixos-system-*-26.05.20260608.bd0ff2d`` → ``"26.05"``.
    """
    m = re.search(r"-(\d{2}\.\d{2})\.\d{8}\.", path)
    return m.group(1) if m else None


def _batch_resolve_nixos_releases(
    local_paths: dict[str, tuple[str, str]],
    deploy_profiles: list[tuple[str, str]],
) -> dict[str, str]:
    """Get NixOS releases for nixos-system profiles in one ``nix eval`` call.

    Only profiles whose *config_type* is ``"nixos-system"`` are fetched.
    Returns a dict ``"node/profile" → release``.
    """
    # Collect nodes that have at least one nixos-system profile.
    nixos_nodes: set[str] = set()
    for node, profile in deploy_profiles:
        key = f"{node}/{profile}"
        entry = local_paths.get(key)
        if entry and entry[1] == "nixos-system":
            nixos_nodes.add(node)

    if not nixos_nodes:
        return {}

    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=0, repo_root=repo_root)

    lines = []
    for node in sorted(nixos_nodes):
        lines.append(f'    "{node}" = configs."{node}".config.system.nixos.release;')
    apply_expr = "configs: {\n" + "\n".join(lines) + "\n}"

    result = runner.run_eval(
        f"{repo_root}#nixosConfigurations",
        json_output=True,
        apply=apply_expr,
    )
    releases = json.loads(result.stdout)

    # Map back to profile keys.
    result_map: dict[str, str] = {}
    for node, profile in deploy_profiles:
        key = f"{node}/{profile}"
        if key in local_paths and local_paths[key][1] == "nixos-system":
            result_map[key] = releases.get(node)

    return result_map


def _should_boot(
    config_type: str,
    boot: bool,
    boot_trigger: Optional[str],
    local_path: str,
    remote_path: Optional[str] = None,
    local_release: Optional[str] = None,
) -> tuple[bool, str]:
    """Decide whether --boot should trigger a reboot.

    Returns ``(should_boot, reason)``.  *reason* is a dimmed note shown
    to the user when boot is skipped despite being requested.
    """
    if not boot:
        return (False, "")
    if config_type != "nixos-system":
        return (False, f"(--boot ignored for {config_type})")
    if boot_trigger == "nixos-release":
        remote_release = None
        if remote_path:
            remote_release = _parse_nixos_release(remote_path)
        if not local_release:
            local_release = _parse_nixos_release(local_path)
        if local_release and remote_release and local_release == remote_release:
            return (
                False,
                f"(--boot skipped: same NixOS release {local_release})",
            )
        return (True, "")
    # No trigger filter — boot always.
    return (True, "")


def _batch_resolve_toplevel_paths(
    deploy_profiles: list[tuple[str, str]],
) -> dict[str, tuple[str, str]]:
    """Resolve underlying config paths for multiple deploy profiles in one call.

    Uses a single 'nix eval' to get all profile derivation paths, then a
    single 'nix derivation show' to extract the underlying config paths
    from structuredAttrs.chosenOutputs. No builds are triggered.

    Returns dict mapping "node/profile" -> (store_path, config_type)
    where config_type is deduced from the underlying path name:
    ``"nixos-system"``, ``"system-manager"``, or ``"home-manager"``.
    """
    repo_root = config.find_repo_root()
    runner = NixRunner(verbosity=0, repo_root=repo_root)

    # Build apply expression that extracts .drvPath for each profile
    lines = []
    for node, profile in deploy_profiles:
        key = f"{node}/{profile}"
        lines.append(f'    "{key}" = nodes."{node}".profiles."{profile}".path.drvPath;')
    apply_expr = "nodes: {\n" + "\n".join(lines) + "\n}"

    # Single nix eval to get all derivation paths
    result = runner.run_eval(
        f"{repo_root}#deploy.nodes",
        json_output=True,
        apply=apply_expr,
    )
    drv_paths = json.loads(result.stdout)

    # Single nix derivation show for all derivations at once
    all_drvs = list(drv_paths.values())
    show_result = subprocess.run(
        ["nix", "derivation", "show"] + all_drvs,
        capture_output=True,
        text=True,
        cwd=repo_root,
    )
    drv_info = json.loads(show_result.stdout)

    # Resolve each profile's underlying config path from
    # structuredAttrs.chosenOutputs (deploy-rs stores the wrapped
    # derivation path there). Fall back to the wrapper's own output.
    paths: dict[str, tuple[str, str]] = {}
    for key, drv_path in drv_paths.items():
        entry = _derivation_lookup(drv_info, drv_path)
        chosen = entry.get("structuredAttrs", {}).get("chosenOutputs", [])
        if chosen and chosen[0].get("paths"):
            underlying = chosen[0]["paths"][0]
        else:
            underlying = entry["outputs"]["out"]["path"]
            if not underlying.startswith("/"):
                underlying = os.path.join("/nix/store", underlying)

        config_type = _classify_config_type(underlying)

        paths[key] = (underlying, config_type)

    return paths


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
    dry_activate: bool = False,
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

    if dry_activate:
        cmd.append("--dry-activate")

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
    boot_trigger: Optional[str] = None,
    no_rollback: bool = False,
    skip_if_unchanged: bool = True,
    verbosity: int = 1,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    dry_activate: bool = False,
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

    ssh_user = get_deploy_ssh_user(target)

    console.print(
        f"[bold]Deploying {target}[/bold]"
        f" ({hostname}, profile: {profile}, user: {ssh_user})"
    )

    # In dry-run mode, resolve the toplevel output path without building
    if dry_run:
        try:
            local_path = get_deploy_toplevel_path(target, profile)
            console.print(f"  Proposed: {local_path}")
        except Exception as e:
            console.print(f"[red]  ERROR: Could not resolve toplevel path: {e}[/red]")
            return False

        config_type = _classify_config_type(local_path)

        # Fetch remote path early if needed for boot-trigger comparison
        _remote_path_for_boot: Optional[str] = None
        if boot and boot_trigger:
            if config_type == "nixos-system":
                _remote_path_for_boot = get_remote_current_system(hostname)
            elif config_type == "system-manager":
                _remote_path_for_boot = get_remote_system_manager_profile(
                    hostname, ssh_user
                )
            else:
                _remote_path_for_boot = get_remote_home_manager_profile(
                    hostname, ssh_user
                )

        if skip_if_unchanged:
            # Reuse remote path if already fetched for boot-trigger
            remote_path: Optional[str]
            if _remote_path_for_boot is not None:
                remote_path = _remote_path_for_boot
            elif config_type == "nixos-system":
                remote_path = get_remote_current_system(hostname)
            elif config_type == "system-manager":
                remote_path = get_remote_system_manager_profile(hostname, ssh_user)
            else:
                remote_path = get_remote_home_manager_profile(hostname, ssh_user)

            if remote_path:
                console.print(f"  Remote:   {remote_path}")
                if local_path == remote_path:
                    console.print(f"  [green]Would skip: already up to date[/green]")
                    return True
                else:
                    console.print(
                        f"  [yellow]Would deploy {target}" f" (changed)[/yellow]"
                    )
            else:
                console.print(
                    f"  [yellow]Would deploy {target}" f" (remote unreachable)[/yellow]"
                )
        else:
            console.print(f"  [yellow]Would deploy {target}[/yellow]")

        should_boot, boot_reason = _should_boot(
            config_type,
            boot,
            boot_trigger,
            local_path,
            _remote_path_for_boot,
        )
        if should_boot:
            console.print("  [yellow]Would boot into new configuration[/yellow]")
        elif boot_reason:
            console.print(f"  [dim]{boot_reason}[/dim]")
        return True

    # Skip if unchanged check (non-dry-run)
    if skip_if_unchanged:
        console.print("  Checking if deployment is needed...")
        # Resolve config type cheaply via eval, then pick the right remote path.
        _local_drv_path = get_deploy_toplevel_path(target, profile)
        _local_config_type = _classify_config_type(_local_drv_path)
        if _local_config_type == "nixos-system":
            remote_path = get_remote_current_system(hostname)
        elif _local_config_type == "system-manager":
            remote_path = get_remote_system_manager_profile(hostname, ssh_user)
        else:
            remote_path = get_remote_home_manager_profile(hostname, ssh_user)

        if remote_path:
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

    # Decide whether to actually boot BEFORE running deploy-rs.
    # When --boot is requested but --boot-trigger says no, we fall
    # back to a live switch so the new config takes effect immediately.
    _do_boot = boot  # may be downgraded to switch below
    if boot and boot_trigger:
        try:
            _local_path = get_deploy_toplevel_path(target, profile)
            _config_type = _classify_config_type(_local_path)
        except Exception:
            _local_path = ""
            _config_type = "nixos-system" if profile == "system" else "home-manager"

        _remote_path: Optional[str] = None
        if boot_trigger == "nixos-release" and _config_type == "nixos-system":
            _remote_path = get_remote_current_system(hostname)

        _do_boot, _reason = _should_boot(
            _config_type,
            boot,
            boot_trigger,
            _local_path,
            _remote_path,
        )
        if not _do_boot and _reason:
            console.print(f"  [dim]{_reason}[/dim]")

    # Run deploy
    mode = "boot" if _do_boot else "switch"
    if dry_activate:
        mode = "dry-activate"
    console.print(f"  Deploying ({mode} mode)...")

    success = run_deploy_command(
        target=target,
        profile=profile,
        boot=_do_boot,
        no_rollback=no_rollback,
        verbosity=verbosity,
        dry_activate=dry_activate,
        extra_nix_args=extra_nix_args,
    )

    if not success:
        console.print(f"[red]  FAILED: Deploy to {target} failed[/red]")
        return False

    # Handle boot mode reboot
    if _do_boot:
        # Record boot_id before triggering reboot for race-free detection
        old_boot_id = get_remote_boot_id(hostname)
        console.print("  Triggering reboot...")
        try:
            subprocess.run(
                [
                    "ssh",
                    "-o",
                    "ControlPath=none",
                    f"{ssh_user}@{hostname}",
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
    boot_trigger: Optional[str] = None,
    no_rollback: bool = False,
    skip_if_unchanged: bool = True,
    stop_on_failure: bool = True,
    verbosity: int = 1,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    dry_activate: bool = False,
    extra_nix_args: Optional[list[str]] = None,
    exclude_pattern: Optional[str] = None,
) -> bool:
    """Deploy all deployable machines.

    Returns True if all succeeded, False if any failed.
    """
    # Check git-crypt
    if not check_git_crypt_unlocked():
        console.print("[red]ERROR: Repository is locked with git-crypt.[/red]")
        console.print("Please unlock it first with: git-crypt unlock")
        return False

    # Get all deploy profiles (node, profile tuples)
    deploy_profiles = ci.get_deploy_profiles()

    if not deploy_profiles:
        console.print("[yellow]No deploy profiles found[/yellow]")
        return True

    # Filter out excluded nodes (exclude entire node regardless of profile)
    if exclude_pattern:
        try:
            exclude_re = re.compile(exclude_pattern)
        except re.error as e:
            console.print(f"[red]Invalid exclude pattern: {e}[/red]")
            return False
        excluded_nodes = {
            node for node, _ in deploy_profiles if exclude_re.search(node)
        }
        deploy_profiles = [
            (n, p) for n, p in deploy_profiles if n not in excluded_nodes
        ]
        if excluded_nodes:
            console.print(
                f"[dim]Excluding {len(excluded_nodes)} machines: {', '.join(sorted(excluded_nodes))}[/dim]"
            )
        if not deploy_profiles:
            console.print("[yellow]No profiles left after exclusions[/yellow]")
            return True

    machine_count = len({n for n, _ in deploy_profiles})
    console.print(
        f"[bold]Deploying {len(deploy_profiles)} profiles "
        f"on {machine_count} machines[/bold]"
    )
    for node, profile in deploy_profiles:
        console.print(f"  - {node}/{profile}")

    # Results tracking
    results: dict[str, bool] = {}
    succeeded = 0
    failed = 0

    # In dry-run mode, kick off batch eval and remote-path collection in
    # the background.  The main loop processes profiles in order; when
    # background data for a profile is ready it takes the fast path
    # (pre-resolved paths), otherwise it falls back to a per-profile
    # dry-run via run_single.
    if dry_run:
        # Pre-resolve hostnames and SSH users for unique nodes (needed
        # by the slow path as well as the remote-futures dispatch).
        nodes_info: dict[str, tuple[str, str]] = {}
        for node in {n for n, _ in deploy_profiles}:
            try:
                nodes_info[node] = (
                    get_deploy_hostname(node),
                    get_deploy_ssh_user(node),
                )
            except Exception:
                nodes_info[node] = (node, "root")

        with ThreadPoolExecutor(max_workers=8) as executor:
            # Kick off batch nix-eval in the background.
            batch_future = executor.submit(
                _batch_resolve_toplevel_paths, deploy_profiles
            )

            # Kick off remote-path collection for every unique node.
            remote_futures: dict[str, "Future"] = {}
            for node, (hostname, ssh_user) in nodes_info.items():
                remote_futures[node] = executor.submit(
                    _collect_all_remote_paths, hostname, ssh_user
                )

            local_paths = None
            batch_releases: dict[str, str] = {}

            for node, profile in deploy_profiles:
                console.print()
                key = f"{node}/{profile}"

                # Try to pick up batch results if they just arrived.
                if local_paths is None and batch_future.done():
                    try:
                        local_paths = batch_future.result()
                        # Resolve NixOS releases for boot-trigger (fast nix eval)
                        if boot_trigger:
                            batch_releases = _batch_resolve_nixos_releases(
                                local_paths, deploy_profiles
                            )
                    except Exception:
                        local_paths = None  # fall through to slow path

                remote_ready = remote_futures[node].done()

                if local_paths is not None and remote_ready:
                    # ── fast path: both data sources ready ──
                    hostname, ssh_user = nodes_info[node]
                    local_path, config_type = local_paths[key]
                    try:
                        all_remote = remote_futures[node].result()
                    except Exception:
                        all_remote = {}
                    remote_path = all_remote.get(config_type)

                    console.print(
                        f"[bold]Deploying {node}[/bold]"
                        f" ({hostname}, profile: {profile}, user: {ssh_user})"
                    )
                    console.print(f"  Proposed: {local_path}")

                    if skip_if_unchanged:
                        if remote_path:
                            console.print(f"  Remote:   {remote_path}")
                            if local_path == remote_path:
                                console.print(
                                    f"  [green]Would skip:"
                                    f" already up to date[/green]"
                                )
                                results[key] = True
                                succeeded += 1
                                continue
                            else:
                                console.print(
                                    f"  [yellow]Would deploy {node}"
                                    f" (changed)[/yellow]"
                                )
                        else:
                            console.print(
                                f"  [yellow]Would deploy {node}"
                                f" (remote unreachable)[/yellow]"
                            )
                    else:
                        console.print(f"  [yellow]Would deploy {node}[/yellow]")

                    should_boot, boot_reason = _should_boot(
                        config_type,
                        boot,
                        boot_trigger,
                        local_path,
                        remote_path,
                        batch_releases.get(key),
                    )
                    if should_boot:
                        console.print(
                            "  [yellow]Would boot into new configuration[/yellow]"
                        )
                    elif boot_reason:
                        console.print(f"  [dim]{boot_reason}[/dim]")
                    results[key] = True
                    succeeded += 1
                else:
                    # ── slow path: fall back to per-profile dry-run ──
                    success = run_single(
                        target=node,
                        profile=profile,
                        boot=boot,
                        boot_trigger=boot_trigger,
                        no_rollback=no_rollback,
                        skip_if_unchanged=skip_if_unchanged,
                        verbosity=verbosity,
                        builders=builders,
                        jobs=jobs,
                        dry_run=True,
                        dry_activate=dry_activate,
                        extra_nix_args=extra_nix_args,
                    )
                    results[key] = success
                    if success:
                        succeeded += 1
                    else:
                        failed += 1
                        if stop_on_failure and not no_rollback:
                            console.print(
                                f"\n[red]Stopping due to" f" failure on {key}[/red]"
                            )
                            break

        # Print summary and return
        console.print()
        console.print("[bold]" + "=" * 40 + "[/bold]")
        if failed > 0:
            console.print(
                f"[bold]Summary:[/bold] {succeeded} would deploy,"
                f" {failed} failed to check"
            )
            console.print("\n[red]Failed to check:[/red]")
            for key, success in results.items():
                if not success:
                    console.print(f"  - {key}")
        else:
            console.print(f"[bold]Summary:[/bold] checked {succeeded} profiles")
        return failed == 0

    # Non-dry-run: run deployments sequentially
    for node, profile in deploy_profiles:
        console.print()
        key = f"{node}/{profile}"
        success = run_single(
            target=node,
            profile=profile,
            boot=boot,
            boot_trigger=boot_trigger,
            no_rollback=no_rollback,
            skip_if_unchanged=skip_if_unchanged,
            verbosity=verbosity,
            builders=builders,
            jobs=jobs,
            dry_run=False,
            dry_activate=dry_activate,
            extra_nix_args=extra_nix_args,
        )
        results[key] = success

        if success:
            succeeded += 1
        else:
            failed += 1
            if stop_on_failure and not no_rollback:
                console.print(f"\n[red]Stopping due to failure on {key}[/red]")
                break

    # Summary
    console.print()
    console.print("[bold]" + "=" * 40 + "[/bold]")
    console.print(f"[bold]Summary:[/bold] {succeeded} succeeded, {failed} failed")

    if failed > 0:
        console.print("\n[red]Failed profiles:[/red]")
        for key, success in results.items():
            if not success:
                console.print(f"  - {key}")

    return failed == 0
