#!/usr/bin/env python3
"""Emit node_exporter textfile metrics mapping PVE LXC volumes to CT identity."""
from __future__ import annotations

import dataclasses
import glob
import os
import subprocess
import sys
import tempfile


@dataclasses.dataclass(frozen=True)
class VolumeRecord:
    vmid: str
    guest: str
    slot: str              # "rootfs", "mp0", ...
    volume: str            # basename, e.g. "subvol-112-disk-0"
    guest_mountpoint: str   # "/", "/data", ...


def parse_ct_config(vmid, text):
    guest = None
    vols = []
    for raw in text.splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or ":" not in line:
            continue
        key, _, val = line.partition(":")
        key, val = key.strip(), val.strip()
        if key == "hostname":
            guest = val
            continue
        if key == "rootfs" or (key.startswith("mp") and key[2:].isdigit()):
            fields = val.split(",")
            first = fields[0]
            # bind mount (absolute path) or non-"storage:volume" entry -> skip
            if first.startswith("/") or ":" not in first:
                continue
            _storage, _, volume = first.partition(":")
            volume = volume.split("/")[-1]
            mp = "/"
            for f in fields[1:]:
                if f.startswith("mp="):
                    mp = f[3:]
            vols.append((key, volume, mp))
    if not guest:
        guest = "ct-%s" % vmid
    return [VolumeRecord(vmid, guest, slot, vol, mp) for (slot, vol, mp) in vols]


_HELP = "# HELP pve_lxc_volume_info Mapping of PVE LXC volumes to container identity."
_TYPE = "# TYPE pve_lxc_volume_info gauge"
_UNMOUNTED = {"", "-", "none", "legacy"}


def parse_zfs_list(output):
    mapping = {}
    for line in output.splitlines():
        if not line.strip():
            continue
        parts = line.split("\t")
        if len(parts) < 2:
            parts = line.split()
        if len(parts) < 2:
            continue
        name, mountpoint = parts[0], parts[1]
        base = name.split("/")[-1]
        if base in mapping and mapping[base][0] != name:
            print(
                "warning: duplicate volume basename %s: %s vs %s"
                % (base, mapping[base][0], name),
                file=sys.stderr,
            )
        mapping[base] = (name, mountpoint)
    return mapping


def _escape(value):
    return value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")


def render_prom(records, zfs_map):
    lines = [_HELP, _TYPE]
    seen = set()
    for r in records:
        entry = zfs_map.get(r.volume)
        if entry is None:
            continue
        dataset, mountpoint = entry
        if mountpoint in _UNMOUNTED:
            continue
        labels = {
            "vmid": r.vmid,
            "guest": r.guest,
            "dataset": dataset,
            "pool": dataset.split("/")[0],
            "slot": r.slot,
            "guest_mountpoint": r.guest_mountpoint,
            "mountpoint": mountpoint,
        }
        items = tuple(sorted(labels.items()))
        if items in seen:
            continue
        seen.add(items)
        label_str = ",".join('%s="%s"' % (k, _escape(v)) for k, v in items)
        lines.append("pve_lxc_volume_info{%s} 1" % label_str)
    return "\n".join(lines) + "\n"


TEXTFILE_DEFAULT = "/var/lib/node_exporter"
CONF_GLOB = "/etc/pve/lxc/*.conf"
OUTPUT_BASENAME = "pve_lxc_volumes.prom"


def collect(conf_paths, zfs_list_output):
    zfs_map = parse_zfs_list(zfs_list_output)
    records = []
    for path in conf_paths:
        vmid = os.path.basename(path)
        if vmid.endswith(".conf"):
            vmid = vmid[: -len(".conf")]
        with open(path, encoding="utf-8") as fh:
            records.extend(parse_ct_config(vmid, fh.read()))
    return render_prom(records, zfs_map)


def _run_zfs_list():
    return subprocess.run(
        ["zfs", "list", "-H", "-o", "name,mountpoint"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout


def _atomic_write(path, content):
    directory = os.path.dirname(path) or "."
    fd, tmp = tempfile.mkstemp(dir=directory, prefix=".pve_lxc_volumes.", suffix=".tmp")
    try:
        with os.fdopen(fd, "w") as fh:
            fh.write(content)
        os.chmod(tmp, 0o644)
        os.replace(tmp, path)
    except BaseException:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def main(argv=None):
    argv = list(sys.argv[1:] if argv is None else argv)
    textfile_dir = argv[0] if argv else TEXTFILE_DEFAULT
    try:
        conf_paths = sorted(glob.glob(CONF_GLOB))
        content = collect(conf_paths, _run_zfs_list())
        _atomic_write(os.path.join(textfile_dir, OUTPUT_BASENAME), content)
    except Exception as exc:
        print("pve-lxc-volume-textfile: failed: %s" % exc, file=sys.stderr)
        raise


if __name__ == "__main__":
    main()
