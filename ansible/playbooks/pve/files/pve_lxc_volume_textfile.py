#!/usr/bin/env python3
"""Emit node_exporter textfile metrics mapping PVE LXC volumes to CT identity."""
from __future__ import annotations

import dataclasses


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
    if guest is None:
        guest = "ct-%s" % vmid
    return [VolumeRecord(vmid, guest, slot, vol, mp) for (slot, vol, mp) in vols]
