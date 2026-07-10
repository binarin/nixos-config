# PVE LXC Disk Monitoring (Phase 1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emit a `pve_lxc_volume_info` node_exporter textfile metric on each PVE host that resolves LXC volume → container hostname and in-guest mountpoint, and surface CT-aware disk usage (host/guest/both) in a Grafana dashboard.

**Architecture:** A stdlib-only Python script parses `/etc/pve/lxc/*.conf` + `zfs list` into a Prometheus info metric written atomically into node_exporter's textfile directory; vmagent ships it to VictoriaMetrics with `bhost` auto-stamped. Grafana joins it into `node_filesystem_*` via `on(bhost,mountpoint) group_left(...)`. Deployment is via the existing ansible PVE monitoring play.

**Tech Stack:** Python 3 (stdlib only, `unittest`), Ansible (`prometheus.prometheus.node_exporter`, `victoriametrics.cluster.vmagent`), systemd timer, VictoriaMetrics, Grafana (`gcx` CLI).

## Global Constraints

- **Merge-safety:** every CT-disk series is identified by `guest` (hostname); never group/legend by `vmid`, `dataset`, or raw `mountpoint` alone.
- **Join key is exactly `(bhost, mountpoint)`**; `mountpoint` label value must equal what node_exporter reports on the host (the ZFS dataset mountpoint path).
- **`bhost` is NOT set by the script** — vmagent injects it via `external_labels`.
- **`guest` = Proxmox CT `hostname:`**; fall back to `ct-<vmid>` when absent.
- **Script writes the whole `.prom` file and installs it via atomic rename every run** — never append/merge.
- **Stdlib only** — no pip dependencies (Debian PVE hosts).
- **LXC only** — no QEMU/VM handling.
- Metric name: `pve_lxc_volume_info`; output file: `<textfile_dir>/pve_lxc_volumes.prom`; textfile dir default `/var/lib/node_exporter`.
- Datasource UID `P4169E866C3094E38` (VictoriaMetrics), type `victoriametrics-metrics-datasource`.
- PVE hosts: `raum`, `valak`, `bael`.

---

### Task 1: CT config parser

**Files:**
- Create: `ansible/playbooks/pve/files/pve_lxc_volume_textfile.py`
- Test: `ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `class VolumeRecord` — frozen dataclass with str fields `vmid, guest, slot, volume, guest_mountpoint`.
  - `parse_ct_config(vmid: str, text: str) -> list[VolumeRecord]` — one record per `rootfs`/`mpN` line that references a `storage:volume` (bind mounts and non-storage lines skipped). `volume` is the basename (e.g. `subvol-112-disk-0`). `guest_mountpoint` is `/` for `rootfs`, else the `mp=` value.

- [ ] **Step 1: Write the failing test**

Create `ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py`:

```python
import unittest
import pve_lxc_volume_textfile as m


class TestParseCtConfig(unittest.TestCase):
    def test_rootfs_and_mountpoint(self):
        text = (
            "arch: amd64\n"
            "hostname: forgejo\n"
            "memory: 4096\n"
            "rootfs: local-zfs:subvol-112-disk-0,size=20G\n"
            "mp0: local-zfs:subvol-112-disk-1,mp=/var/lib/data,size=100G\n"
            "net0: name=eth0,bridge=vmbr0\n"
        )
        recs = m.parse_ct_config("112", text)
        self.assertEqual(
            recs,
            [
                m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/"),
                m.VolumeRecord("112", "forgejo", "mp0", "subvol-112-disk-1", "/var/lib/data"),
            ],
        )

    def test_missing_hostname_falls_back(self):
        recs = m.parse_ct_config("107", "rootfs: local-zfs:subvol-107-disk-0,size=8G\n")
        self.assertEqual(recs[0].guest, "ct-107")

    def test_bind_mount_and_non_storage_skipped(self):
        text = (
            "hostname: media\n"
            "rootfs: local-zfs:subvol-111-disk-0,size=8G\n"
            "mp0: /srv/host/path,mp=/data\n"          # bind mount, no storage:volume
            "mp1: spinning:subvol-111-disk-5,mp=/media/tube\n"
        )
        recs = m.parse_ct_config("111", text)
        self.assertEqual([r.slot for r in recs], ["rootfs", "mp1"])
        self.assertEqual(recs[1].volume, "subvol-111-disk-5")
        self.assertEqual(recs[1].guest_mountpoint, "/media/tube")


if __name__ == "__main__":
    unittest.main()
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`
Expected: FAIL with `ModuleNotFoundError: No module named 'pve_lxc_volume_textfile'`.

- [ ] **Step 3: Write minimal implementation**

Create `ansible/playbooks/pve/files/pve_lxc_volume_textfile.py`:

```python
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
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add ansible/playbooks/pve/files/pve_lxc_volume_textfile.py ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py
git commit -m "feat(pve-disk): CT config parser for LXC volume identity"
```

---

### Task 2: ZFS list parsing + Prometheus rendering

**Files:**
- Modify: `ansible/playbooks/pve/files/pve_lxc_volume_textfile.py`
- Test: `ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py`

**Interfaces:**
- Consumes: `VolumeRecord`, `parse_ct_config` (Task 1).
- Produces:
  - `parse_zfs_list(output: str) -> dict[str, tuple[str, str]]` — maps volume basename → `(dataset_full_name, host_mountpoint)`. Input is `zfs list -H -o name,mountpoint` output (tab-separated). On duplicate basename, last wins and a warning is printed to stderr.
  - `render_prom(records: list[VolumeRecord], zfs_map: dict[str, tuple[str, str]]) -> str` — Prometheus exposition text (with `# HELP`/`# TYPE`), one `pve_lxc_volume_info{...} 1` line per record whose `volume` resolves to a mounted dataset. Records whose volume is absent from the map, or whose mountpoint is `""`/`-`/`none`/`legacy`, are skipped. Duplicate label sets are de-duplicated. Trailing newline. Label values escaped per Prometheus rules (`\`, `"`, newline).

- [ ] **Step 1: Write the failing test**

Append to `ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py` (before the `if __name__` block):

```python
class TestParseZfsList(unittest.TestCase):
    def test_maps_basename_to_dataset_and_mountpoint(self):
        out = (
            "rpool\t/rpool\n"
            "rpool/data/subvol-112-disk-0\t/rpool/data/subvol-112-disk-0\n"
            "spinning/pve-data/subvol-111-disk-5\t/spinning/pve-data/subvol-111-disk-5\n"
        )
        mp = m.parse_zfs_list(out)
        self.assertEqual(
            mp["subvol-112-disk-0"],
            ("rpool/data/subvol-112-disk-0", "/rpool/data/subvol-112-disk-0"),
        )
        self.assertEqual(
            mp["subvol-111-disk-5"],
            ("spinning/pve-data/subvol-111-disk-5", "/spinning/pve-data/subvol-111-disk-5"),
        )


class TestRenderProm(unittest.TestCase):
    def _map(self):
        return {
            "subvol-112-disk-0": ("rpool/data/subvol-112-disk-0", "/rpool/data/subvol-112-disk-0"),
            "subvol-112-disk-1": ("rpool/data/subvol-112-disk-1", "/rpool/data/subvol-112-disk-1"),
        }

    def test_emits_expected_line(self):
        recs = [m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/")]
        out = m.render_prom(recs, self._map())
        self.assertIn("# TYPE pve_lxc_volume_info gauge", out)
        self.assertIn(
            'pve_lxc_volume_info{'
            'dataset="rpool/data/subvol-112-disk-0",'
            'guest="forgejo",'
            'guest_mountpoint="/",'
            'mountpoint="/rpool/data/subvol-112-disk-0",'
            'pool="rpool",'
            'slot="rootfs",'
            'vmid="112"} 1',
            out,
        )
        self.assertTrue(out.endswith("\n"))

    def test_unresolved_volume_skipped(self):
        recs = [m.VolumeRecord("999", "ghost", "rootfs", "subvol-999-disk-0", "/")]
        out = m.render_prom(recs, self._map())
        self.assertNotIn("ghost", out)

    def test_unmounted_dataset_skipped(self):
        recs = [m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/")]
        out = m.render_prom(recs, {"subvol-112-disk-0": ("rpool/data/subvol-112-disk-0", "-")})
        self.assertNotIn("forgejo", out)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`
Expected: FAIL with `AttributeError: module 'pve_lxc_volume_textfile' has no attribute 'parse_zfs_list'`.

- [ ] **Step 3: Write minimal implementation**

Add `import sys` to the top imports of `pve_lxc_volume_textfile.py` (change `import dataclasses` block to include `sys`), then append these functions:

```python
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
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`
Expected: PASS (7 tests).

- [ ] **Step 5: Commit**

```bash
git add ansible/playbooks/pve/files/pve_lxc_volume_textfile.py ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py
git commit -m "feat(pve-disk): zfs list parsing and prometheus rendering"
```

---

### Task 3: Collection assembly + atomic writer + CLI entrypoint

**Files:**
- Modify: `ansible/playbooks/pve/files/pve_lxc_volume_textfile.py`
- Test: `ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py`

**Interfaces:**
- Consumes: `parse_ct_config`, `parse_zfs_list`, `render_prom` (Tasks 1–2).
- Produces:
  - `collect(conf_paths: list[str], zfs_list_output: str) -> str` — reads each config file path (filename stem = vmid), parses all, renders the combined `.prom` text.
  - `main(argv=None)` — resolves textfile dir from `argv[0]` (default `/var/lib/node_exporter`), runs `zfs list -H -o name,mountpoint`, and atomically writes `<dir>/pve_lxc_volumes.prom`.
  - Executable via `python3 pve_lxc_volume_textfile.py [textfile_dir]`.

- [ ] **Step 1: Write the failing test**

Append to the test file (before the `if __name__` block):

```python
import os
import tempfile


class TestCollect(unittest.TestCase):
    def test_reads_configs_and_renders(self):
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "112.conf"), "w") as fh:
                fh.write("hostname: forgejo\nrootfs: local-zfs:subvol-112-disk-0,size=20G\n")
            zfs = "rpool/data/subvol-112-disk-0\t/rpool/data/subvol-112-disk-0\n"
            out = m.collect([os.path.join(d, "112.conf")], zfs)
            self.assertIn('guest="forgejo"', out)
            self.assertIn('vmid="112"', out)


class TestAtomicWrite(unittest.TestCase):
    def test_writes_file_contents(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "out.prom")
            m._atomic_write(path, "hello\n")
            with open(path) as fh:
                self.assertEqual(fh.read(), "hello\n")
            # no leftover temp files
            self.assertEqual(os.listdir(d), ["out.prom"])
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`
Expected: FAIL with `AttributeError: module ... has no attribute 'collect'`.

- [ ] **Step 3: Write minimal implementation**

Add `glob`, `os`, `subprocess`, `tempfile` to imports (top of file), then append:

```python
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
        with open(path) as fh:
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
    conf_paths = sorted(glob.glob(CONF_GLOB))
    content = collect(conf_paths, _run_zfs_list())
    _atomic_write(os.path.join(textfile_dir, OUTPUT_BASENAME), content)


if __name__ == "__main__":
    main()
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`
Expected: PASS (9 tests).

- [ ] **Step 5: Make the script executable and commit**

```bash
chmod +x ansible/playbooks/pve/files/pve_lxc_volume_textfile.py
git add ansible/playbooks/pve/files/pve_lxc_volume_textfile.py ansible/playbooks/pve/files/test_pve_lxc_volume_textfile.py
git update-index --chmod=+x ansible/playbooks/pve/files/pve_lxc_volume_textfile.py
git commit -m "feat(pve-disk): config collection, zfs invocation and atomic textfile writer"
```

---

### Task 4: Systemd units + ansible deployment

**Files:**
- Create: `ansible/playbooks/pve/files/pve-lxc-volume-textfile.service`
- Create: `ansible/playbooks/pve/files/pve-lxc-volume-textfile.timer`
- Create: `ansible/playbooks/pve/tasks/lxc-volume-textfile.yml`
- Modify: `ansible/playbooks/pve/vars/main.yml`
- Modify: `ansible/playbooks/pve/main.yml`

**Interfaces:**
- Consumes: `pve_lxc_volume_textfile.py` (Tasks 1–3), the `node_exporter_textfile_dir` role variable, `/var/lib/node_exporter` textfile directory (created by the node_exporter role).
- Produces: a periodically-refreshed `/var/lib/node_exporter/pve_lxc_volumes.prom` on every PVE host.

- [ ] **Step 1: Create the systemd service unit**

Create `ansible/playbooks/pve/files/pve-lxc-volume-textfile.service`:

```ini
[Unit]
Description=Export PVE LXC volume -> container identity mapping for node_exporter
After=zfs.target
Wants=zfs.target

[Service]
Type=oneshot
ExecStart=/usr/local/bin/pve-lxc-volume-textfile /var/lib/node_exporter
```

- [ ] **Step 2: Create the systemd timer unit**

Create `ansible/playbooks/pve/files/pve-lxc-volume-textfile.timer`:

```ini
[Unit]
Description=Periodic refresh of PVE LXC volume identity mapping

[Timer]
OnBootSec=2min
OnUnitActiveSec=5min
AccuracySec=30s

[Install]
WantedBy=timers.target
```

- [ ] **Step 3: Create the ansible tasks file**

Create `ansible/playbooks/pve/tasks/lxc-volume-textfile.yml`:

```yaml
---
- name: Install pve-lxc-volume-textfile script
  ansible.builtin.copy:
    src: pve_lxc_volume_textfile.py
    dest: /usr/local/bin/pve-lxc-volume-textfile
    mode: "0755"

- name: Install pve-lxc-volume-textfile systemd service
  ansible.builtin.copy:
    src: pve-lxc-volume-textfile.service
    dest: /etc/systemd/system/pve-lxc-volume-textfile.service
    mode: "0644"

- name: Install pve-lxc-volume-textfile systemd timer
  ansible.builtin.copy:
    src: pve-lxc-volume-textfile.timer
    dest: /etc/systemd/system/pve-lxc-volume-textfile.timer
    mode: "0644"

- name: Enable and start pve-lxc-volume-textfile timer
  ansible.builtin.systemd:
    name: pve-lxc-volume-textfile.timer
    enabled: true
    state: started
    daemon_reload: true

- name: Run pve-lxc-volume-textfile once to populate metrics now
  ansible.builtin.systemd:
    name: pve-lxc-volume-textfile.service
    state: started
```

- [ ] **Step 4: Set the node_exporter textfile directory variable**

In `ansible/playbooks/pve/vars/main.yml`, add this line under the existing top-level keys (after `zfs_exporter_version`):

```yaml
node_exporter_textfile_dir: /var/lib/node_exporter
```

**Verify the variable name** against the installed collection before applying:

Run: `ansible-galaxy collection list prometheus.prometheus`
Then: `grep -rn 'textfile' ~/.ansible/collections/ansible_collections/prometheus/prometheus/roles/node_exporter/ 2>/dev/null | head`
Expected: a `defaults/main.yml` entry named `node_exporter_textfile_dir` and a template using `--collector.textfile.directory`. If the name differs in v0.27.4, use the actual variable name here.

- [ ] **Step 5: Wire the tasks file into the monitoring play**

In `ansible/playbooks/pve/main.yml`, the monitoring play currently is:

```yaml
- hosts: pve
  tags:
    - monitoring
  vars_files:
    - vars/main.yml

  roles:
    - victoriametrics.cluster.vmagent
    - prometheus.prometheus.node_exporter
    - aroberts.zfs_exporter
```

Add a `tasks:` section so it becomes:

```yaml
- hosts: pve
  tags:
    - monitoring
  vars_files:
    - vars/main.yml

  roles:
    - victoriametrics.cluster.vmagent
    - prometheus.prometheus.node_exporter
    - aroberts.zfs_exporter

  tasks:
    - import_tasks: tasks/lxc-volume-textfile.yml
```

- [ ] **Step 6: Lint the ansible changes**

Run: `cd ansible && ansible-lint playbooks/pve/main.yml playbooks/pve/tasks/lxc-volume-textfile.yml`
Expected: no errors (warnings about `copy` vs `template` are acceptable).

- [ ] **Step 7: Commit**

```bash
git add ansible/playbooks/pve/files/pve-lxc-volume-textfile.service \
        ansible/playbooks/pve/files/pve-lxc-volume-textfile.timer \
        ansible/playbooks/pve/tasks/lxc-volume-textfile.yml \
        ansible/playbooks/pve/vars/main.yml \
        ansible/playbooks/pve/main.yml
git commit -m "feat(pve-disk): deploy lxc volume textfile exporter via ansible"
```

---

### Task 5: Deploy to PVE hosts and verify the metric pipeline

**Files:** none (operational task).

**Interfaces:**
- Consumes: everything from Tasks 1–4.
- Produces: verified `pve_lxc_volume_info` series in VictoriaMetrics for all PVE hosts.

- [ ] **Step 1: Enable the node_exporter textfile collector on raum**

Run: `cd ansible && ansible-playbook playbooks/pve/main.yml --tags monitoring --limit raum`
Expected: changed tasks for the node_exporter role (textfile flag) and the four `lxc-volume-textfile` tasks; no failures.

- [ ] **Step 2: Confirm the .prom file exists and is well-formed on raum**

Run: `ssh root@raum 'cat /var/lib/node_exporter/pve_lxc_volumes.prom | head -20'`
Expected: `# HELP`/`# TYPE` lines followed by `pve_lxc_volume_info{...guest="forgejo"...} 1` lines. No Python traceback.

- [ ] **Step 3: Confirm node_exporter serves the metric on raum**

Run: `ssh root@raum 'curl -s localhost:9100/metrics | grep -c pve_lxc_volume_info'`
Expected: a non-zero count (roughly the number of CT volumes on raum).

- [ ] **Step 4: Confirm the metric reached VictoriaMetrics with bhost**

Run:
```bash
Q=$(python3 -c "import urllib.parse;print(urllib.parse.quote('pve_lxc_volume_info{bhost=\"raum\",guest=\"forgejo\"}'))")
gcx api "/api/datasources/proxy/uid/P4169E866C3094E38/api/v1/query?query=$Q" -o json | head -c 800
```
Expected: a result with labels including `bhost="raum"`, `guest="forgejo"`, `mountpoint="/rpool/data/subvol-112-disk-0"`. (Allow ~30s for the scrape+remote-write.)

- [ ] **Step 5: Verify the join returns CT-labeled usage**

Run:
```bash
EXPR='node_filesystem_avail_bytes{mountpoint=~"/rpool/data/subvol.*"} * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info'
Q=$(python3 -c "import urllib.parse,sys;print(urllib.parse.quote(sys.argv[1]))" "$EXPR")
gcx api "/api/datasources/proxy/uid/P4169E866C3094E38/api/v1/query?query=$Q" -o json | python3 -c 'import sys,json;r=sys.stdin.read();d=json.loads(r[r.index("{"):]);res=d["data"]["result"];print("joined series:",len(res));print(res[0]["metric"] if res else "NONE")'
```
Expected: `joined series: > 0`, and the sample metric carries `guest`, `vmid`, `guest_mountpoint`. No `many-to-many`/`group_left` error.

- [ ] **Step 6: Roll out to valak and bael**

Run: `cd ansible && ansible-playbook playbooks/pve/main.yml --tags monitoring --limit valak,bael`
Then confirm counts:
```bash
for h in valak bael; do
  Q=$(python3 -c "import urllib.parse;print(urllib.parse.quote('count(pve_lxc_volume_info{bhost=\"'$h'\"})'))")
  echo -n "$h: "; gcx api "/api/datasources/proxy/uid/P4169E866C3094E38/api/v1/query?query=$Q" -o json | python3 -c 'import sys,json;r=sys.stdin.read();d=json.loads(r[r.index("{"):]);res=d["data"]["result"];print(res[0]["value"][1] if res else 0)'
done
```
Expected: non-zero counts for both hosts.

- [ ] **Step 7: Commit rollout notes (optional)**

No code change; if you keep an ops log, record the per-host volume counts. Otherwise skip.

---

### Task 6: Phase-1 Grafana dashboard with host/guest/both toggle

**Files:**
- Create: `dashboards/pve-lxc-disk.json` (working copy of the dashboard model, for reproducibility; not yet wired into declarative provisioning)

**Interfaces:**
- Consumes: `pve_lxc_volume_info` join (Task 5), datasource UID `P4169E866C3094E38`.
- Produces: a Grafana dashboard `pve-lxc-disk` driven by a `$source` variable.

**Reusable `$source` gate.** The three-way toggle uses a custom variable `source` with values `host`, `guest`, `both`, and this pure-PromQL gate appended to each source branch so only the selected branch(es) emit series (works without a recording rule):

```
* on() group_left() (label_replace(vector(1), "v", "$source", "", ""))
```
…combined with a branch-specific matcher. Concretely the two branches are:

- **host branch** (gate matches when `$source` is `host` or `both`):
```
(
  100 * (1 - node_filesystem_avail_bytes{mountpoint=~"/rpool/data/subvol.*"}
             / node_filesystem_size_bytes{mountpoint=~"/rpool/data/subvol.*"})
  * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info
)
* on() group_left() (label_replace(vector(1), "v", "$source", "", "")){v=~"host|both"}
```
- **guest branch** (gate matches when `$source` is `guest` or `both`; `bhost` copied to `guest`):
```
label_replace(
  100 * (1 - node_filesystem_avail_bytes{job="node",mountpoint!~"/rpool/data/subvol.*",fstype!~"tmpfs|fuse.*|vfat|overlay|ramfs|nsfs|autofs"}
             / node_filesystem_size_bytes{job="node",mountpoint!~"/rpool/data/subvol.*",fstype!~"tmpfs|fuse.*|vfat|overlay|ramfs|nsfs|autofs"}),
  "guest", "$__auto", "", "")
* on() group_left() (label_replace(vector(1), "v", "$source", "", "")){v=~"guest|both"}
```
(For the guest branch, set the panel legend/label to `{{bhost}}` since `bhost` is the CT name there; the `label_replace` `guest` copy is best-effort and the merge-safety rule still holds because `bhost` uniquely identifies the guest.)

- [ ] **Step 1: Build the dashboard model JSON**

Create `dashboards/pve-lxc-disk.json` verbatim. (Design choices: the table and bargauge are host-authoritative — the CT-aware view is the whole point — and gated `host|both`; the timeseries carries both host and guest branches; panel 1 is the Phase-1 free-bytes signal that stands in for the deferred alert. The `$DS` placeholder below is the datasource object, repeated on every target — it is `{"type":"victoriametrics-metrics-datasource","uid":"P4169E866C3094E38"}`.)

```json
{
  "dashboard": {
    "uid": "pve-lxc-disk",
    "title": "PVE LXC Disk",
    "tags": ["disk", "pve", "lxc"],
    "timezone": "browser",
    "refresh": "1m",
    "time": { "from": "now-24h", "to": "now" },
    "templating": {
      "list": [
        {
          "name": "source", "label": "Source", "type": "custom",
          "query": "host,guest,both",
          "current": { "text": "both", "value": "both" },
          "options": [
            { "text": "host", "value": "host" },
            { "text": "guest", "value": "guest" },
            { "text": "both", "value": "both", "selected": true }
          ]
        }
      ]
    },
    "panels": [
      {
        "id": 1, "type": "stat", "title": "Volumes < 5 GiB free (host)",
        "gridPos": { "h": 4, "w": 6, "x": 0, "y": 0 },
        "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
        "targets": [{
          "refId": "A",
          "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
          "expr": "count(min by (bhost, mountpoint) (node_filesystem_avail_bytes{mountpoint=~\"/rpool/data/subvol.*\"}) < 5 * 1024 * 1024 * 1024) or vector(0)",
          "instant": true
        }],
        "options": { "colorMode": "background", "graphMode": "none", "reduceOptions": { "calcs": ["lastNotNull"] } },
        "fieldConfig": { "defaults": { "thresholds": { "mode": "absolute", "steps": [ { "color": "green", "value": null }, { "color": "red", "value": 1 } ] } }, "overrides": [] }
      },
      {
        "id": 2, "type": "stat", "title": "Least free space (worst volume, host)",
        "gridPos": { "h": 4, "w": 6, "x": 6, "y": 0 },
        "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
        "targets": [{
          "refId": "A",
          "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
          "expr": "min(min by (bhost, mountpoint) (node_filesystem_avail_bytes{mountpoint=~\"/rpool/data/subvol.*\"}))",
          "instant": true
        }],
        "options": { "colorMode": "background", "graphMode": "none", "reduceOptions": { "calcs": ["lastNotNull"] } },
        "fieldConfig": { "defaults": { "unit": "bytes", "thresholds": { "mode": "absolute", "steps": [ { "color": "red", "value": null }, { "color": "yellow", "value": 5368709120 }, { "color": "green", "value": 21474836480 } ] } }, "overrides": [] }
      },
      {
        "id": 3, "type": "bargauge", "title": "Disk usage % by guest (host / $source)",
        "gridPos": { "h": 20, "w": 12, "x": 0, "y": 4 },
        "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
        "targets": [{
          "refId": "A",
          "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
          "expr": "(100 * (1 - node_filesystem_avail_bytes{mountpoint=~\"/rpool/data/subvol.*\"} / node_filesystem_size_bytes{mountpoint=~\"/rpool/data/subvol.*\"}) * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info) * on() group_left() (label_replace(vector(1),\"v\",\"$source\",\"\",\"\")){v=~\"host|both\"}",
          "instant": true,
          "legendFormat": "{{guest}}  {{guest_mountpoint}}"
        }],
        "options": { "displayMode": "gradient", "orientation": "horizontal", "showUnfilled": true, "valueMode": "text" },
        "fieldConfig": { "defaults": { "unit": "percent", "min": 0, "max": 100, "thresholds": { "mode": "absolute", "steps": [ { "color": "green", "value": null }, { "color": "yellow", "value": 70 }, { "color": "orange", "value": 85 }, { "color": "red", "value": 95 } ] } }, "overrides": [] }
      },
      {
        "id": 4, "type": "table", "title": "CT filesystems (host / $source)",
        "gridPos": { "h": 20, "w": 12, "x": 12, "y": 4 },
        "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
        "targets": [
          {
            "refId": "A",
            "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
            "expr": "(100 * (1 - node_filesystem_avail_bytes{mountpoint=~\"/rpool/data/subvol.*\"} / node_filesystem_size_bytes{mountpoint=~\"/rpool/data/subvol.*\"}) * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info) * on() group_left() (label_replace(vector(1),\"v\",\"$source\",\"\",\"\")){v=~\"host|both\"}",
            "instant": true, "format": "table"
          },
          {
            "refId": "B",
            "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
            "expr": "(node_filesystem_avail_bytes{mountpoint=~\"/rpool/data/subvol.*\"} * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info) * on() group_left() (label_replace(vector(1),\"v\",\"$source\",\"\",\"\")){v=~\"host|both\"}",
            "instant": true, "format": "table"
          },
          {
            "refId": "C",
            "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
            "expr": "(node_filesystem_size_bytes{mountpoint=~\"/rpool/data/subvol.*\"} * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info) * on() group_left() (label_replace(vector(1),\"v\",\"$source\",\"\",\"\")){v=~\"host|both\"}",
            "instant": true, "format": "table"
          }
        ],
        "transformations": [
          { "id": "merge", "options": {} },
          { "id": "organize", "options": { "excludeByName": { "Time": true, "__name__": true, "device": true, "fstype": true, "instance": true, "job": true, "pool": true, "slot": true, "vmid": true }, "renameByName": { "Value #A": "Used %", "Value #B": "Free", "Value #C": "Size", "guest": "Guest", "guest_mountpoint": "Mount", "bhost": "Host" } } },
          { "id": "sortBy", "options": { "fields": {}, "sort": [ { "field": "Used %", "desc": true } ] } }
        ],
        "fieldConfig": {
          "defaults": {},
          "overrides": [
            { "matcher": { "id": "byName", "options": "Used %" }, "properties": [ { "id": "unit", "value": "percent" }, { "id": "decimals", "value": 1 }, { "id": "custom.cellOptions", "value": { "type": "color-background" } }, { "id": "thresholds", "value": { "mode": "absolute", "steps": [ { "color": "green", "value": null }, { "color": "yellow", "value": 70 }, { "color": "orange", "value": 85 }, { "color": "red", "value": 95 } ] } } ] },
            { "matcher": { "id": "byName", "options": "Free" }, "properties": [ { "id": "unit", "value": "bytes" } ] },
            { "matcher": { "id": "byName", "options": "Size" }, "properties": [ { "id": "unit", "value": "bytes" } ] }
          ]
        }
      },
      {
        "id": 5, "type": "timeseries", "title": "Disk usage % over time ($source)",
        "gridPos": { "h": 12, "w": 24, "x": 0, "y": 24 },
        "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
        "targets": [
          {
            "refId": "A",
            "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
            "expr": "(100 * (1 - node_filesystem_avail_bytes{mountpoint=~\"/rpool/data/subvol.*\"} / node_filesystem_size_bytes{mountpoint=~\"/rpool/data/subvol.*\"}) * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info) * on() group_left() (label_replace(vector(1),\"v\",\"$source\",\"\",\"\")){v=~\"host|both\"}",
            "legendFormat": "host: {{guest}}  {{guest_mountpoint}}"
          },
          {
            "refId": "B",
            "datasource": { "type": "victoriametrics-metrics-datasource", "uid": "P4169E866C3094E38" },
            "expr": "(100 * (1 - node_filesystem_avail_bytes{job=\"node\",mountpoint!~\"/rpool/data/subvol.*\",fstype!~\"tmpfs|fuse.*|vfat|overlay|ramfs|nsfs|autofs\"} / node_filesystem_size_bytes{job=\"node\",mountpoint!~\"/rpool/data/subvol.*\",fstype!~\"tmpfs|fuse.*|vfat|overlay|ramfs|nsfs|autofs\"})) * on() group_left() (label_replace(vector(1),\"v\",\"$source\",\"\",\"\")){v=~\"guest|both\"}",
            "legendFormat": "guest: {{bhost}}  {{mountpoint}}"
          }
        ],
        "options": { "legend": { "displayMode": "table", "placement": "right", "calcs": ["lastNotNull", "max"] }, "tooltip": { "mode": "multi", "sort": "desc" } },
        "fieldConfig": { "defaults": { "unit": "percent", "min": 0, "max": 100, "custom": { "drawStyle": "line", "fillOpacity": 5, "showPoints": "never" }, "thresholds": { "mode": "absolute", "steps": [ { "color": "green", "value": null }, { "color": "red", "value": 85 } ] } }, "overrides": [] }
      }
    ]
  },
  "overwrite": true,
  "message": "phase1 CT-aware disk dashboard (issue #181)"
}
```

- [ ] **Step 2: Create the dashboard in Grafana**

Run: `gcx api /api/dashboards/db -d @dashboards/pve-lxc-disk.json`
Expected: JSON with `"status":"success"` and `"uid":"pve-lxc-disk"`.

- [ ] **Step 3: Verify each toggle value returns data**

Run:
```bash
for v in host guest both; do
  EXPR='(100 * (1 - node_filesystem_avail_bytes{mountpoint=~"/rpool/data/subvol.*"} / node_filesystem_size_bytes{mountpoint=~"/rpool/data/subvol.*"}) * on(bhost,mountpoint) group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info) * on() group_left() (label_replace(vector(1),"v","'$v'","",""))'
  Q=$(python3 -c "import urllib.parse,sys;print(urllib.parse.quote(sys.argv[1]))" "$EXPR")
  echo -n "source=$v host-branch series: "
  gcx api "/api/datasources/proxy/uid/P4169E866C3094E38/api/v1/query?query=$Q" -o json | python3 -c 'import sys,json;r=sys.stdin.read();d=json.loads(r[r.index("{"):]);print(len(d["data"]["result"]))'
done
```
Expected: `host` and `both` return the same non-zero host-branch count; `guest` returns 0 for the host branch (its series come from the guest branch instead). This confirms the gate works.

- [ ] **Step 4: Manual visual check**

Open `https://monitor.lynx-lizard.ts.net/d/pve-lxc-disk`, switch `$source` between host/guest/both, confirm the table shows friendly `Guest`/`Mount` columns (e.g. `forgejo` / `/`) and that a fullest-first ordering is visible.

- [ ] **Step 5: Commit the dashboard model**

```bash
git add dashboards/pve-lxc-disk.json
git commit -m "feat(pve-disk): phase-1 CT-aware disk dashboard with source toggle"
```

---

## Out of scope (future Phase 2 plan)

- Grafana-managed recording rule producing unified `ct:disk_usage:pct{guest,vmid,mountpoint,source}` remote-written to VictoriaMetrics.
- **Notifying** Grafana alert rule on absolute free bytes (`avail < 5Gi`), keyed by `guest`. Deferred out of Phase 1 for two concrete reasons found during planning: (1) `gcx alert rules` is read-only, so rule creation needs the Grafana provisioning API — which pairs naturally with the Phase-2 declarative provisioning; (2) the only existing contact point is a placeholder (`example@email.com`), so a real notification channel must be chosen first. Phase 1 instead surfaces the free-bytes signal as a dashboard stat panel (Task 6, panel 1) so the "which volumes are nearly full" question is answerable immediately.
- Optional `predict_linear` "will fill in N days" panel/alert.
- Declarative provisioning of the dashboard via the NixOS Grafana module on `monitor`.

## Notes for the implementer

- Run the Python tests from inside `ansible/playbooks/pve/files/` so the module import resolves: `cd ansible/playbooks/pve/files && python3 -m unittest test_pve_lxc_volume_textfile -v`.
- The PVE hosts are Debian; the script must stay stdlib-only. Do not add pip installs.
- If `ansible-lint` flags `command`/`copy` idioms, prefer fixing rather than disabling unless it fights the existing repo conventions (check `ansible/.ansible-lint`).
- Task 5 and Task 6 require network access to the PVE hosts and the Grafana/VM instance; they are verification-heavy rather than TDD.
