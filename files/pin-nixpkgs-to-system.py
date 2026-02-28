#!/usr/bin/env python3
"""
Pin flake.lock nixpkgs to the current NixOS system revision.

This script updates flake.lock to use the same nixpkgs revision as the
running NixOS system, while keeping flake.nix with an unversioned
github:NixOS/nixpkgs reference.

Usage:
    pin-nixpkgs-to-system.py [FLAKE_DIR]

If FLAKE_DIR is not specified, uses the current directory.
"""

import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path


def get_system_nixpkgs_revision() -> str:
    """Get the nixpkgs revision of the running NixOS system."""
    result = subprocess.run(
        ["nixos-version", "--json"],
        capture_output=True,
        text=True,
        check=True,
    )
    data = json.loads(result.stdout)
    return data["nixpkgsRevision"]


def read_file(path: Path) -> str:
    return path.read_text()


def write_file(path: Path, content: str) -> None:
    path.write_text(content)


def update_nixpkgs_url(flake_nix: str, revision: str | None) -> str:
    """
    Update nixpkgs.url in flake.nix.

    If revision is None, use unversioned github:NixOS/nixpkgs.
    Otherwise, pin to the specific revision.
    """
    if revision:
        new_url = f'github:NixOS/nixpkgs/{revision}'
    else:
        new_url = 'github:NixOS/nixpkgs'

    # Match nixpkgs.url = "..." with various possible formats
    pattern = r'(nixpkgs\.url\s*=\s*)"[^"]+";'
    replacement = f'\\1"{new_url}";'

    new_content, count = re.subn(pattern, replacement, flake_nix)
    if count == 0:
        raise ValueError("Could not find nixpkgs.url in flake.nix")

    return new_content


def run_flake_lock(flake_dir: Path) -> None:
    """Run nix flake lock in the given directory."""
    subprocess.run(
        ["nix", "flake", "lock"],
        cwd=flake_dir,
        check=True,
        capture_output=True,
    )


def update_lock_nixpkgs(lock_path: Path, pinned_lock: dict) -> None:
    """
    Update flake.lock with nixpkgs info from pinned_lock,
    but keep the original section without rev in it.
    """
    current_lock = json.loads(read_file(lock_path))
    pinned_nixpkgs = pinned_lock["nodes"]["nixpkgs"]["locked"]

    # Update locked section with pinned values
    current_lock["nodes"]["nixpkgs"]["locked"] = {
        "lastModified": pinned_nixpkgs["lastModified"],
        "narHash": pinned_nixpkgs["narHash"],
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": pinned_nixpkgs["rev"],
        "type": "github",
    }

    # Keep original without rev (unversioned)
    current_lock["nodes"]["nixpkgs"]["original"] = {
        "owner": "NixOS",
        "repo": "nixpkgs",
        "type": "github",
    }

    write_file(lock_path, json.dumps(current_lock, indent=2) + "\n")


def main() -> int:
    # Parse arguments
    if len(sys.argv) > 2:
        print(f"Usage: {sys.argv[0]} [FLAKE_DIR]", file=sys.stderr)
        return 1

    flake_dir = Path(sys.argv[1]) if len(sys.argv) == 2 else Path.cwd()
    flake_nix_path = flake_dir / "flake.nix"
    flake_lock_path = flake_dir / "flake.lock"

    if not flake_nix_path.exists():
        print(f"Error: {flake_nix_path} not found", file=sys.stderr)
        return 1

    # Get system nixpkgs revision
    try:
        system_rev = get_system_nixpkgs_revision()
    except (subprocess.CalledProcessError, json.JSONDecodeError, KeyError) as e:
        print(f"Error getting system nixpkgs revision: {e}", file=sys.stderr)
        return 1

    print(f"System nixpkgs revision: {system_rev}")

    # Save original flake.nix
    original_flake_nix = read_file(flake_nix_path)

    try:
        # Step 1: Pin to system revision and lock
        print("Pinning to system revision...")
        pinned_flake_nix = update_nixpkgs_url(original_flake_nix, system_rev)
        write_file(flake_nix_path, pinned_flake_nix)
        run_flake_lock(flake_dir)

        # Save the pinned lock
        pinned_lock = json.loads(read_file(flake_lock_path))

        # Step 2: Revert to unversioned and lock again
        print("Reverting to unversioned nixpkgs...")
        unversioned_flake_nix = update_nixpkgs_url(original_flake_nix, None)
        write_file(flake_nix_path, unversioned_flake_nix)
        run_flake_lock(flake_dir)

        # Step 3: Update lock with pinned values
        print("Updating flake.lock with pinned revision...")
        update_lock_nixpkgs(flake_lock_path, pinned_lock)

        print(f"Done. flake.lock now uses nixpkgs {system_rev}")
        return 0

    except Exception as e:
        # Restore original on error
        print(f"Error: {e}", file=sys.stderr)
        print("Restoring original flake.nix...", file=sys.stderr)
        write_file(flake_nix_path, original_flake_nix)
        return 1


if __name__ == "__main__":
    sys.exit(main())
