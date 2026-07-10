# PVE LXC disk monitoring — CT-aware filesystem metrics

Date: 2026-07-10
Issue: forgejo #181 (free disk space monitoring)
Status: design approved, pending spec review

## Problem

Disk-space metrics from the Proxmox (PVE) hosts are hard to read and partly
redundant:

1. **Opaque volume names.** On a PVE host, each LXC container volume shows up as
   `node_filesystem_*{mountpoint="/rpool/data/subvol-112-disk-0"}`. There is no
   way to tell that `subvol-112-disk-0` is the `forgejo` container's root disk,
   or which in-guest path a secondary disk (`subvol-112-disk-1`) is mounted at.
2. **CT/host duplication.** A NixOS container reports its own filesystems (`/`,
   `/nix/store`, …) via its in-guest node_exporter *and* the same underlying
   dataset is reported by the PVE host as `/rpool/data/subvol-N-disk-M`. The two
   views coexist with no shared identifier.

The mapping needed to fix (1) — container id → hostname, and disk slot → real
in-guest mountpoint — lives only in the Proxmox CT config files
(`/etc/pve/lxc/<vmid>.conf`), which Prometheus/vmagent relabeling cannot read.

## Goals

- Attach a human-friendly `guest` (CT hostname) label and the real
  `guest_mountpoint` to the PVE-host filesystem metrics.
- Keep **both** the host-side and in-guest views; let a dashboard variable
  select `host` / `guest` / `both`.
- Provide free-disk-space **alerting** across all monitored machines (issue #181).
- Be robust to CT-id reuse over long time ranges (no accidental series merge).

## Non-goals

- QEMU/KVM VM disks. Their storage is zvols/block devices; the host cannot see
  in-VM filesystem fill, so they are out of scope.
- Replacing or reconfiguring the existing `aroberts.zfs_exporter` (kept as-is;
  the new `dataset` label leaves the door open to join with `zfs_dataset_*`
  later, but that is not part of this work).

## Current environment (as explored)

- **PVE hosts** (`raum`, `valak`, `bael`) — Debian, configured via `ansible/`
  (`playbooks/pve/main.yml`, monitoring play). They run:
  - `prometheus.prometheus.node_exporter` (collection v0.27.4) → `node_filesystem_*`
  - `aroberts.zfs_exporter` v1.0.0 on `:9134` → `zfs_dataset_*`, `zfs_pool_*`
  - `victoriametrics.cluster.vmagent` (v2.37.0) → scrapes both, stamps
    `external_labels.bhost = {{ inventory_hostname }}`, remote-writes to VM.
- **`monitor` host** — NixOS (`modules/machines/monitor.nix`). Runs
  VictoriaMetrics single-node (`services.victoriametrics`, `0.0.0.0:8428`) and
  Grafana. **No `vmalert`** → recording/alerting rules must be Grafana-managed.
- **NixOS guests** — `modules/monitored.nix` (`nixos-config.export-metrics`):
  node_exporter + vmagent, `bhost = hostname`. node_exporter `textfile`
  collector is available (not disabled).
- Datasource: `VictoriaMetrics`, uid `P4169E866C3094E38`, type
  `victoriametrics-metrics-datasource` (default). `hledger` postgres is the other.

### Key facts established during exploration

- On the PVE host each CT volume is mounted **once** at
  `/rpool/data/subvol-N-disk-M` (no mountpoint fan-out — that only happens
  *inside* NixOS guests), and `node_filesystem_size_bytes` there reflects the
  **refquota** (the configured disk size), so host-side `% used` is correct.
- `zfs_dataset_quota_bytes` is `0` (Proxmox sets `refquota`, which
  zfs_exporter v1.0.0 does not emit) — so the size denominator must come from
  node_exporter, not the zfs_exporter.
- **Stopped CTs stay visible on the host**: ZFS mounts the subvol dataset at
  `/rpool/data/subvol-N-disk-M` via the dataset `mountpoint` property regardless
  of container run state; stopping only tears down the bind-mount into the
  container namespace. The config file also survives a stop. Only **destroying**
  a CT removes both the config and the dataset.

## Design

### Component 1 — `pve_lxc_volume_info` (PVE host, ansible)

An info metric, one series per CT volume (rootfs + each `mpN`), value `1`:

```
pve_lxc_volume_info{
  bhost="raum",                                 # auto-injected by vmagent (PVE host)
  vmid="112",
  guest="forgejo",                              # from CT config `hostname:`
  dataset="rpool/data/subvol-112-disk-0",       # full zfs dataset path
  pool="rpool",
  slot="rootfs",                                # rootfs | mp0 | mp1 | …
  guest_mountpoint="/",                         # mount path *inside* the CT
  mountpoint="/rpool/data/subvol-112-disk-0"    # host mountpoint == node_filesystem label
} 1
```

- **Join key: `(bhost, mountpoint)`.** `bhost` is stamped by vmagent;
  `mountpoint` is copied verbatim from what node_exporter reports on the host, so
  consumers use `... * on(bhost, mountpoint) group_left(guest, vmid,
  guest_mountpoint, slot) pve_lxc_volume_info` with no `label_replace`.
- `bhost` is **not** added by the script — vmagent's `external_labels` inject it
  on every scraped series including node_exporter textfile output.

**Producer script** (`pve-lxc-volume-textfile`), run by a systemd timer:

1. Enumerate CTs from `/etc/pve/lxc/*.conf` (filename stem → `vmid`).
2. `hostname:` → `guest`.
3. For `rootfs:` and each `mpN:` line, parse
   `<storage>:<volume>,…,mp=<path>` → `slot`, volume ref, `guest_mountpoint`
   (`rootfs` ⇒ `/`).
4. Resolve volume → host dataset + host mountpoint via a single
   `zfs list -H -o name,mountpoint`, matching by volume basename. (Avoids
   re-implementing `storage.cfg` pool mapping; ZFS is the source of truth for
   the actual host mountpoint.)
5. Emit one `pve_lxc_volume_info` line per volume.

**Hard requirements on the script:**

- **Whole-file regenerate + atomic rename** every run (never append/merge). This
  is what makes CT-id reuse safe: a destroyed/renamed CT's old series stops
  getting samples and goes stale within one interval, so it drops out of instant
  queries cleanly. (See "CT-id reuse" below.)
- Emit straight from `zfs list`; do **not** special-case stopped CTs. A stopped
  CT is still host-mounted → still emitted and joined (desirable: it still
  occupies disk). An unmounted volume simply won't match a base series.
- Skip volumes on non-ZFS storage. Tolerate missing `hostname:` (fall back to
  `guest="ct-<vmid>"`). Tolerate manually-named datasets
  (e.g. `subvol--docker-on-nixos--root`) — basename match still works.

**Deployment (ansible, `playbooks/pve/main.yml` monitoring play):**

- Set `node_exporter_textfile_dir` on the `prometheus.prometheus.node_exporter`
  role (default `/var/lib/node_exporter`; auto-adds
  `--collector.textfile.directory`). *Verify exact var name against collection
  v0.27.4 during implementation.*
- Install the script + a `.service` and `.timer` (interval ~5 min; the job is
  cheap). node_exporter restart handler fires only if the flag changed.

**Interface / testability:** the script's whole contract is "read CT configs +
`zfs list` → print prom text to stdout." Unit-testable against `.conf` fixtures
with no Proxmox present; integration-testable by running on a host and
inspecting the `.prom`.

### Component 2 — Consumption (Grafana), phased

**Merge-safety rule (load-bearing).** Every CT-disk series is identified by
`guest` (hostname) — never by `vmid`, `dataset`, or raw `mountpoint` alone.
Because the enriched value carries `guest` from a *time-varying* info metric, a
reused id 112 renders as two distinct series (`guest="forgejo"` era and
`guest="newthing"` era) that cannot fuse on a long-range graph. Dashboards and
alerts therefore always drive off the joined, `guest`-labeled form and legend by
`guest`; `vmid` is informational only. Graphing the raw
`node_filesystem{mountpoint="/rpool/data/subvol-112…"}` directly, or grouping by
`vmid`, is the one thing that would re-merge eras and is disallowed by
convention.

**Two sources, one uniform shape:**

- **host** —
  `node_filesystem_*{mountpoint=~"/rpool/data/subvol.*"} * on(bhost,mountpoint)
  group_left(guest,vmid,guest_mountpoint) pve_lxc_volume_info`. Covers every CT
  incl. Debian-only ones and stopped CTs; one row per volume; correct
  refquota-based size; `guest`-labeled.
- **guest** — each CT's own `node_filesystem_*` (its `/`, `/nix/store`, …), where
  `bhost` already *is* the CT name; `label_replace(bhost → guest)` so legends
  line up with the host source.

#### Phase 1 — dashboard-only (experimental, no new infra)

- Each panel carries both source queries, tagged `source="host"` / `"guest"` via
  `label_replace`, unioned with `or`. A Grafana **custom** variable `$source`
  (`host` / `guest` / `both` → regex) selects which shows.
- Panels: fullest-volumes bar gauge, table (`guest` · `guest_mountpoint` ·
  used % · free · size), usage-% timeseries — all legended by `guest`.
- **Alerting**: Grafana alert on **absolute free bytes** as primary
  (`avail < 5Gi`), `% used` as secondary context. Keyed by `guest`. Absolute
  bytes are chosen because for near-full ZFS the byte headroom matters and it
  sidesteps the pool-shared-denominator ambiguity of ZFS `% used`.
- Iterate on this experimentally (the current throwaway `disk-free-space`
  dashboard is the starting point) before committing anything declaratively.

#### Phase 2 — Grafana-managed recording rule (once labels settle)

- Bake the join into a unified metric `ct:disk_usage:pct{guest, vmid,
  guest_mountpoint, mountpoint, bhost, source}` (+ a `ct:disk_avail:bytes`
  companion), evaluated by a Grafana-managed recording rule and remote-written
  back to VictoriaMetrics.
- The `$source` variable collapses to a trivial `source=~"$source"` matcher;
  alert rules become one-liners.
- Requires configuring a Grafana → VM remote-write target and rule evaluation.
- Declarative adoption: fold the ansible bits (already declarative) plus the
  Grafana dashboard/rules provisioning into the repo. (Grafana on `monitor` is
  already provisioned via NixOS; recording rules / alert rules can follow the
  same provisioning path.)

## CT-id reuse — correctness argument

- `guest`/`vmid` are labels on a **time-varying** info metric, so
  `{vmid="112", guest="forgejo"}` and a later `{vmid="112", guest="other"}` are
  distinct series. Point-in-time queries at any past timestamp resolve the join
  to whoever owned id 112 *then*.
- The whole-file-regenerate + atomic-rename requirement ensures a destroyed CT's
  old info series stops immediately and goes stale (~5 min), so instant
  queries/alerts see only the current owner.
- Residual edge case (accepted): a range query crossing the exact reuse instant
  can transiently double-match within the staleness window → a brief
  many-to-one join hiccup, guardable with `max by(...)` if it ever bites. The
  user has explicitly deprioritized this transient; the long-timescale
  no-merge property (guaranteed by the `guest` label) is what matters.

## Testing

- **Unit**: parser against `.conf` fixtures — rootfs, `mpN`, non-ZFS storage,
  manually-named dataset, missing `hostname:`.
- **Host smoke**: run the script on `raum`; `cat` the `.prom`;
  `curl :9100/metrics | grep pve_lxc_volume_info`; confirm it appears in VM with
  `bhost`; verify the join in Grafana Explore for a known CT (112 → forgejo).
- **Dashboard**: confirm `$source` toggles host/guest/both and that a reused id
  renders as separate `guest` lines over a long range.

## Rollout

1. Land ansible change; apply to `raum` first (most CTs), then `valak`, `bael`.
2. Build/iterate the Phase-1 dashboard + free-bytes alert.
3. Once labels are stable, implement Phase 2 recording rule + declarative
   dashboard/alert provisioning.

## Open items to confirm during implementation

- Exact `node_exporter_textfile_dir` variable name/behavior in
  `prometheus.prometheus.node_exporter` v0.27.4.
- Language for the producer script (POSIX sh + `awk`, or Python — Debian hosts
  have Python 3). Leaning Python for testable parsing.
- Whether `guest_mountpoint` for `rootfs` should be `/` or the CT's configured
  rootfs path (Proxmox rootfs is always `/`).
