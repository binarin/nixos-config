#!/usr/bin/env python3
"""ZL-0016 regression: run from the repo root; optional committed flake URL argv[1]."""
import copy
import json
from pathlib import Path
import subprocess
import sys

BASE = "ddd24f978466d434bd2a74fb942dca06dd8ecd80"
COMMON = "3a5edaff90fd342ec8b5f4cc11817f494c62fae8"
SHARED = "nixpkgs nixpkgs-unstable flake-parts import-tree disko home-manager sops-nix emacs-tramp-rpc emacs-overlay".split()


def command(*args):
    return subprocess.check_output(args, text=True)


def resolve(lock, edge):
    if isinstance(edge, str):
        return lock["nodes"][edge]
    node = lock["nodes"][lock["root"]]
    for name in edge:
        node = resolve(lock, node["inputs"][name])
    return node


baseline = json.loads(command("git", "show", f"{BASE}:flake.lock"))
source = Path(
    json.loads(
        command(
            "nix",
            "flake",
            "metadata",
            "--json",
            "--no-write-lock-file",
            sys.argv[1] if len(sys.argv) > 1 else ".",
        )
    )["path"]
)
lock = json.loads((source / "flake.lock").read_text())
assert "?ref=swan-lake-current" in (source / "flake.nix").read_text()
root = lock["nodes"][lock["root"]]["inputs"]
common_node = resolve(lock, root["common"])
assert common_node["locked"]["rev"] == COMMON
assert common_node["original"]["ref"] == "swan-lake-current"
metadata = json.loads(
    command(
        "nix",
        "flake",
        "metadata",
        "--json",
        "--no-write-lock-file",
        f"git+https://forgejo.lynx-lizard.ts.net/binarin/swan-lake-common?rev={COMMON}",
    )
)
common = json.loads((Path(metadata["path"]) / "flake.lock").read_text())


def retained(candidate, old, new):
    # Node IDs may change; compare the complete non-shared graph by resolved edges.
    assert {k: v for k, v in old.items() if k != "inputs"} == {
        k: v for k, v in new.items() if k != "inputs"
    }
    assert old.get("inputs", {}).keys() == new.get("inputs", {}).keys()
    for name, edge in old.get("inputs", {}).items():
        other = new["inputs"][name]
        if isinstance(edge, list):
            assert edge == other, name
        else:
            retained(candidate, resolve(baseline, edge), resolve(candidate, other))


def check(candidate):
    inputs = candidate["nodes"][candidate["root"]]["inputs"]
    assert set(inputs) == set(baseline["nodes"][baseline["root"]]["inputs"]) | {
        "common"
    }
    for name in SHARED:
        assert inputs[name] == ["common", name], name
        assert (
            resolve(candidate, inputs[name])["locked"]
            == resolve(common, [name])["locked"]
        ), name
    for name, edge in baseline["nodes"][baseline["root"]]["inputs"].items():
        if name not in SHARED:
            retained(
                candidate, resolve(baseline, edge), resolve(candidate, inputs[name])
            )


check(lock)
for mutation in ("follow", "locked"):
    divergent = copy.deepcopy(lock)
    if mutation == "follow":
        divergent["nodes"][divergent["root"]]["inputs"]["nixpkgs"] = [
            "common",
            "nixpkgs-unstable",
        ]
    else:
        resolve(divergent, ["nixpkgs"])["locked"]["rev"] = "0" * 40
    try:
        check(divergent)
    except AssertionError:
        print(f"PASS: divergent {mutation} rejected")
    else:
        raise AssertionError(f"divergent {mutation} accepted")

print(
    "PASS: nine full common locks/follows, 23 unrelated root graphs and retained flake-file"
)
