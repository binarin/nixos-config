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


def run_nixos(
    configuration: str,
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> None:
    """Build a NixOS configuration.

    Args:
        configuration: The NixOS configuration name to build
        output: Output path for the result symlink (None = no output link)
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
        extra_nix_args: Extra arguments to pass to nix build
    """
    repo_root = config.find_repo_root()

    flake_ref = (
        f"{repo_root}#nixosConfigurations.{configuration}.config.system.build.toplevel"
    )

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        if output:
            console.print(f"[bold]Output:[/bold] {output}")
        if builders:
            console.print(f"[bold]Builders:[/bold] {', '.join(builders)}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
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

    runner.run_build(flake_ref, output=output, extra_args=extra_nix_args)
    if output:
        console.print(f"[green]✓[/green] Built {configuration} -> {output}")
    else:
        console.print(f"[green]✓[/green] Built {configuration}")


def run_home(
    host: str,
    user: str,
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> None:
    """Build a home-manager configuration.

    Args:
        host: The host name
        user: The user name
        output: Output path for the result symlink (None = no output link)
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
        extra_nix_args: Extra arguments to pass to nix build
    """
    repo_root = config.find_repo_root()

    flake_ref = f"{repo_root}#nixosConfigurations.{host}.config.home-manager.users.{user}.home.activationPackage"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        if output:
            console.print(f"[bold]Output:[/bold] {output}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
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

    runner.run_build(flake_ref, output=output, extra_args=extra_nix_args)
    if output:
        console.print(f"[green]✓[/green] Built {host}/{user} -> {output}")
    else:
        console.print(f"[green]✓[/green] Built {host}/{user}")


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
    compression: str = "zstd",
    extra_nix_args: Optional[list[str]] = None,
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
        compression: Output compression format ('zstd' or 'xz')
        extra_nix_args: Extra arguments to pass to nix build
    """
    repo_root = config.find_repo_root()

    # Determine output path with correct extension
    if output is None:
        ext = _get_output_extension(compression)
        output = repo_root / f"proxmox-lxc-{target}{ext}"

    flake_ref = f"{repo_root}#nixosConfigurations.{target}.config.system.build.tarball"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        console.print(f"[bold]Output:[/bold] {output}")
        console.print(f"[bold]Compression:[/bold] {compression}")
        if inject_secrets:
            if fake_secrets:
                console.print("[bold]Would inject fake secrets (placeholders)[/bold]")
            else:
                console.print("[bold]Would inject secrets[/bold]")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
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
            runner.run_build(flake_ref, output=temp_output, extra_args=extra_nix_args)

            # Find the actual tarball in the nix store result
            tarball_path = _find_tarball_in_result(temp_output)

            if fake_secrets:
                console.print("[bold]Injecting fake secrets into tarball...[/bold]")
            else:
                console.print("[bold]Injecting secrets into tarball...[/bold]")
            _inject_secrets_into_tarball(
                target, tarball_path, output, runner, fake_secrets, compression
            )

        suffix = " (fake)" if fake_secrets else ""
        console.print(
            f"[green]✓[/green] Built {target} with secrets{suffix} -> {output}"
        )
    else:
        runner.run_build(flake_ref, output=output, extra_args=extra_nix_args)
        console.print(f"[green]✓[/green] Built {target} -> {output}")


def _find_tarball_in_result(result_path: Path) -> Path:
    """Find the tarball (.tar.xz or .tar.zst) in a nix build result.

    The nix build result for system.build.tarball is a directory
    containing the tarball, possibly in a nested subdirectory.
    """
    if result_path.is_symlink():
        result_path = result_path.resolve()

    # Look for .tar.zst or .tar.xz file recursively in the result directory
    if result_path.is_dir():
        # Try zstd first (preferred), then xz
        for ext in ["*.tar.zst", "*.tar.xz"]:
            tarballs = list(result_path.glob(f"**/{ext}"))
            if tarballs:
                return tarballs[0]

    raise ExternalToolError(
        "tarball", f"Could not find .tar.xz or .tar.zst tarball in {result_path}"
    )


def _detect_compression(tarball_path: Path) -> str:
    """Detect compression format from tarball extension.

    Returns:
        'zstd' or 'xz'
    """
    if tarball_path.suffix == ".zst" or str(tarball_path).endswith(".tar.zst"):
        return "zstd"
    return "xz"


def _get_decompress_command(compression: str) -> list[str]:
    """Get the decompression command for a given format."""
    if compression == "zstd":
        return ["zstd", "-dc"]
    return ["xz", "-dc"]


def _get_compress_command(compression: str) -> list[str]:
    """Get the compression command for a given format."""
    if compression == "zstd":
        return ["zstd", "-T0"]
    return ["xz", "-T0"]


def _get_output_extension(compression: str) -> str:
    """Get the file extension for a given compression format."""
    if compression == "zstd":
        return ".tar.zst"
    return ".tar.xz"


def _group_secrets_by_owner(
    secrets: list,
) -> dict[tuple[str, str], list]:
    """Group secrets by (owner, group) tuple.

    Args:
        secrets: List of SecretFile objects

    Returns:
        Dict mapping (owner, group) to list of secrets
    """
    from collections import defaultdict

    groups: dict[tuple[str, str], list] = defaultdict(list)
    for secret in secrets:
        key = (secret.owner, secret.group)
        groups[key].append(secret)
    return dict(groups)


def _inject_secrets_into_tarball(
    machine_name: str,
    source_tarball: Path,
    output_path: Path,
    runner: NixRunner,
    fake_secrets: bool = False,
    compression: str = "zstd",
) -> None:
    """Inject decrypted secrets into a tarball using streaming.

    This function uses bsdtar for efficient streaming:
    1. Gathers secrets configuration from NixOS config
    2. Groups secrets by owner/group
    3. Creates small temp tar archives for each ownership group using GNU tar
    4. Streams: decompress source | bsdtar (combine archives) | compress to output

    This avoids writing the large decompressed tarball to disk.

    Args:
        machine_name: The NixOS configuration name
        source_tarball: Path to the source .tar.xz or .tar.zst tarball
        output_path: Path for the output tarball with secrets
        runner: NixRunner instance for nix eval
        fake_secrets: Use placeholder content instead of decrypting
        compression: Output compression format ('zstd' or 'xz')
    """
    # Gather secrets
    secrets = gather_secrets_for_machine(machine_name, runner)
    if not secrets:
        console.print("[yellow]Warning: No secrets found to inject[/yellow]")
        # Still copy the tarball to output
        shutil.copy2(source_tarball, output_path)
        return

    console.print(f"  Found {len(secrets)} secret(s) to inject")

    # Group secrets by owner/group for separate tar archives
    secret_groups = _group_secrets_by_owner(secrets)
    console.print(f"  Ownership groups: {len(secret_groups)}")

    # Track temp directories and files for cleanup
    temp_dirs: list[Path] = []
    temp_tar_files: list[Path] = []

    try:
        # Create small tar archives for each ownership group
        console.print("  Preparing secrets archives...")
        for (owner, group), group_secrets in secret_groups.items():
            # Create secrets in temp directory for this group
            if fake_secrets:
                secrets_dir = generate_fake_secrets_to_tempdir(group_secrets)
            else:
                secrets_dir = decrypt_secrets_to_tempdir(group_secrets)
            temp_dirs.append(secrets_dir)

            # Create a small tar archive with correct ownership using GNU tar
            with tempfile.NamedTemporaryFile(
                prefix=f"ncf-secrets-{owner}-", suffix=".tar", delete=False
            ) as temp_tar:
                temp_tar_path = Path(temp_tar.name)
                temp_tar_files.append(temp_tar_path)

            subprocess.run(
                [
                    "tar",
                    "cf",
                    str(temp_tar_path),
                    f"--owner={owner}",
                    f"--group={group}",
                    "-C",
                    str(secrets_dir),
                    ".",
                ],
                check=True,
            )

        # Build the streaming pipeline:
        # decompress | bsdtar cf - @- @secrets1.tar @secrets2.tar | compress > output
        console.print("  Streaming injection pipeline...")

        # Detect source compression
        source_compression = _detect_compression(source_tarball)
        decompress_cmd = _get_decompress_command(source_compression)
        compress_cmd = _get_compress_command(compression)

        # Build bsdtar command with @- (stdin) and @file for each secrets archive
        bsdtar_cmd = ["bsdtar", "cf", "-", "@-"]
        for tar_file in temp_tar_files:
            bsdtar_cmd.append(f"@{tar_file}")

        # Run the pipeline: decompress | bsdtar | compress > output
        with open(output_path, "wb") as out_file:
            # Start decompressor
            decompress_proc = subprocess.Popen(
                decompress_cmd + [str(source_tarball)],
                stdout=subprocess.PIPE,
            )

            # Start bsdtar, reading from decompressor
            bsdtar_proc = subprocess.Popen(
                bsdtar_cmd,
                stdin=decompress_proc.stdout,
                stdout=subprocess.PIPE,
            )
            # Allow decompress_proc to receive SIGPIPE if bsdtar exits
            if decompress_proc.stdout:
                decompress_proc.stdout.close()

            # Start compressor, reading from bsdtar
            compress_proc = subprocess.Popen(
                compress_cmd,
                stdin=bsdtar_proc.stdout,
                stdout=out_file,
            )
            # Allow bsdtar_proc to receive SIGPIPE if compress exits
            if bsdtar_proc.stdout:
                bsdtar_proc.stdout.close()

            # Wait for all processes
            compress_rc = compress_proc.wait()
            bsdtar_rc = bsdtar_proc.wait()
            decompress_rc = decompress_proc.wait()

            if decompress_rc != 0:
                raise ExternalToolError(
                    decompress_cmd[0],
                    f"Decompression failed with exit code {decompress_rc}",
                    decompress_rc,
                )
            if bsdtar_rc != 0:
                raise ExternalToolError(
                    "bsdtar", f"bsdtar failed with exit code {bsdtar_rc}", bsdtar_rc
                )
            if compress_rc != 0:
                raise ExternalToolError(
                    compress_cmd[0],
                    f"Compression failed with exit code {compress_rc}",
                    compress_rc,
                )

    finally:
        # Clean up temp tar files
        for tar_file in temp_tar_files:
            if tar_file.exists():
                tar_file.unlink()
        # Clean up all secrets directories
        for temp_dir in temp_dirs:
            shutil.rmtree(temp_dir, ignore_errors=True)


def run_iso(
    output: Optional[Path] = None,
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> None:
    """Build ISO image.

    Args:
        output: Output path for the result symlink (None = no output link)
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs
        dry_run: Show what would be done without building
        extra_nix_args: Extra arguments to pass to nix build
    """
    repo_root = config.find_repo_root()

    flake_ref = f"{repo_root}#nixosConfigurations.iso.config.system.build.isoImage"

    if dry_run:
        console.print(f"[bold]Would build:[/bold] {flake_ref}")
        if output:
            console.print(f"[bold]Output:[/bold] {output}")
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
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

    runner.run_build(flake_ref, output=output, extra_args=extra_nix_args)
    if output:
        console.print(f"[green]✓[/green] Built ISO -> {output}")
    else:
        console.print(f"[green]✓[/green] Built ISO")


def run_all(
    verbosity: int = 1,
    use_nom: Optional[bool] = None,
    builders: Optional[list[str]] = None,
    jobs: str = "auto",
    max_parallel: Optional[int] = None,
    dry_run: bool = False,
    extra_nix_args: Optional[list[str]] = None,
) -> None:
    """Build all NixOS configurations.

    Args:
        verbosity: 0=quiet, 1=normal, 2=verbose
        use_nom: Use nix-output-monitor (None=auto-detect)
        builders: List of remote builders
        jobs: Number of parallel jobs for nix
        max_parallel: Max parallel builds (default: CPU count)
        dry_run: Show what would be done without building
        extra_nix_args: Extra arguments to pass to nix build
    """
    repo_root = config.find_repo_root()

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
        if extra_nix_args:
            console.print(f"[bold]Extra nix args:[/bold] {' '.join(extra_nix_args)}")
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
            build_runner.run_build(flake_ref, output=None, extra_args=extra_nix_args)
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
