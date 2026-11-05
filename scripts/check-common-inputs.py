#!/usr/bin/env python3
"""ZL-0016 regression: run from the repo root; optional committed flake URL argv[1]."""
import copy
import json
from pathlib import Path
import subprocess
import sys

BASE = "82ba0a2abd984a3e8564403986479685c20082d7"
COMMON = "8ab98a22587c201047388823e92d377eb7da899b"
SHARED = "nixpkgs nixpkgs-unstable flake-parts import-tree disko home-manager sops-nix emacs-tramp-rpc".split()
NIXOS = "acme claude-nixos-config demandred devcontainer docker-on-nixos forgejo furfur garage iso llm-runner mail media metabase monitor niks3 nix-builder-raum nix-builder-valak paperless-nixos pi-box postgres qdevice social web xray-exit xray-front zeroclaw".split()


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
lock = json.loads(Path("flake.lock").read_text())
root = lock["nodes"][lock["root"]]["inputs"]
common_node = resolve(lock, root["common"])
assert common_node["locked"]["rev"] == COMMON
metadata = json.loads(command("nix", "flake", "metadata", "--json", "--no-write-lock-file",
                              f"git+https://forgejo.lynx-lizard.ts.net/binarin/swan-lake-common?rev={COMMON}"))
common = json.loads((Path(metadata["path"]) / "flake.lock").read_text())


def check(candidate):
    inputs = candidate["nodes"][candidate["root"]]["inputs"]
    assert set(inputs) == (set(baseline["nodes"][baseline["root"]]["inputs"]) - {"flake-file"}) | {"common"}
    for name in SHARED:
        assert inputs[name] == ["common", name], name
        assert resolve(candidate, inputs[name])["locked"] == resolve(common, [name])["locked"], name
    for name, edge in baseline["nodes"][baseline["root"]]["inputs"].items():
        if name not in SHARED + ["flake-file"]:
            old, new = resolve(baseline, edge), resolve(candidate, inputs[name])
            assert old["locked"] == new["locked"], name
            assert {k: v for k, v in old.get("inputs", {}).items() if isinstance(v, list)} == {
                k: v for k, v in new.get("inputs", {}).items() if isinstance(v, list)}, name
    assert not any(n.get("locked", {}).get("repo") == "flake-file" for n in candidate["nodes"].values())


check(lock)
for mutation in ("follow", "locked"):
    divergent = copy.deepcopy(lock)
    if mutation == "follow":
        divergent["nodes"][divergent["root"]]["inputs"]["nixpkgs"] = ["common", "nixpkgs-unstable"]
    else:
        resolve(divergent, ["nixpkgs"])["locked"]["rev"] = "0" * 40
    try:
        check(divergent)
    except AssertionError:
        print(f"PASS: divergent {mutation} rejected")
    else:
        raise AssertionError(f"divergent {mutation} accepted")

for filename in command("git", "ls-files", "-z").split("\0"):
    path = Path(filename)
    if not path.is_file() or filename == __file__ or filename == "scripts/check-common-inputs.py":
        continue
    text = path.read_bytes()
    if any(token in text for token in (b"flake-file", b"write-flake", b"autofollow")):
        assert b"Historical generation instructions superseded by ZL-0016:" in text, filename

source = sys.argv[1] if len(sys.argv) > 1 else "."
for output, expected in (("nixosConfigurations", NIXOS), ("systemConfigs", ["b-db-k", "b-db2-k", "murmur"]),
                         ("packages", ["aarch64-linux", "x86_64-linux"])):
    actual = json.loads(command("nix", "eval", "--no-write-lock-file", "--json", f"{source}#{output}",
                                "--apply", "builtins.attrNames"))
    assert actual == sorted(expected), (output, actual)
print("PASS: eight full shared locks/follows, 24 retained pins/follows, systems, outputs, generation removal")
