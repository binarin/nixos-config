#!/usr/bin/env python3
"""pve-memguard: kill whole PVE guests in rank order under host-wide memory pressure.

SL-0081, design from SL-45 research section L3. Run as `python3 -I`.

- A PSI trigger on /proc/pressure/memory wakes the daemon.
- The kill only happens when host-wide headroom (MemAvailable + ARC above
  c_min + SwapFree) is below a threshold, so a CT stalling against its own
  memcg limit is ignored.
- One guest is killed per pressure event, by writing to its cgroup.kill (no
  pct/qm, which are Perl and need pmxcfs). Then the daemon settles before it
  escalates.
- A killed guest is started again (`pct start` / `qm start` in a transient
  systemd unit) once the restart delay is over and host pressure is low. While
  pressure stays high it retries. After `kill_cap` kills within
  `kill_cap_window_seconds` the guest is held stopped and exported as an alert
  metric. The kill history is persisted, so a daemon restart does not reset
  the cap.
- In dry-run mode it logs what it would kill and changes nothing.
"""
from __future__ import annotations

import collections
import ctypes
import dataclasses
import json
import os
import select
import subprocess
import sys
import tempfile
import time

GIB = 1 << 30

# Marks the tier in `kill_order` that also takes every running guest that
# is not listed anywhere.
DEFAULT_TIER_MARKER = "default"

DEFAULT_CONFIG = {
    "dry_run": True,
    "psi_trigger_kind": "full",
    "psi_trigger_stall_us": 1_000_000,
    "psi_trigger_window_us": 10_000_000,
    "headroom_threshold_bytes": 3 * GIB,
    "settle_seconds": 15,
    "exit_grace_seconds": 120,
    "restart_delay_seconds": 600,
    "restart_retry_seconds": 300,
    "restart_max_some_avg10": 5.0,
    "restart_min_headroom_bytes": 6 * GIB,
    "kill_cap": 3,
    "kill_cap_window_seconds": 3600,
    # Least important first: the first running guest in this order is
    # killed first.
    "kill_order": [
        ["ct:123"],
        ["ct:111"],
        ["ct:116"],
        ["ct:103", "ct:107", "ct:121", "ct:127", "ct:129", "vm:104", DEFAULT_TIER_MARKER],
        ["ct:124"],
        ["vm:109"],
        ["ct:112"],
        ["ct:115"],
        ["ct:101"],
    ],
    "state_file": "/var/lib/pve-memguard/state.json",
    "textfile_dir": "/var/lib/node_exporter",
}

PRESSURE_DECISIONS = (
    "headroom_ok",    # pressure, but the host as a whole has headroom
    "awaiting_exit",  # headroom will come back once killed guests finish exiting
    "kill",
    "would_kill",     # dry run
    "kill_failed",
    "no_victim",      # nothing left to kill
)

METRICS_BASENAME = "pve_memguard.prom"
STATE_VERSION = 1
MAX_POLL_SECONDS = 60


@dataclasses.dataclass(frozen=True, order=True)
class Guest:
    kind: str  # "ct" or "vm"
    vmid: int

    @classmethod
    def parse(cls, ref):
        kind, sep, vmid = ref.partition(":")
        if not sep or kind not in ("ct", "vm") or not (vmid.isascii() and vmid.isdigit()):
            raise ValueError("bad guest %r, expected ct:<vmid> or vm:<vmid>" % (ref,))
        return cls(kind, int(vmid))

    def __str__(self):
        return "%s:%d" % (self.kind, self.vmid)

    @property
    def label(self):
        return "%s %d" % (self.kind.upper(), self.vmid)


@dataclasses.dataclass(frozen=True)
class Config:
    dry_run: bool
    psi_trigger_kind: str
    psi_trigger_stall_us: int
    psi_trigger_window_us: int
    headroom_threshold_bytes: int
    settle_seconds: int
    exit_grace_seconds: int
    restart_delay_seconds: int
    restart_retry_seconds: int
    restart_max_some_avg10: float
    restart_min_headroom_bytes: int
    kill_cap: int
    kill_cap_window_seconds: int
    kill_order: tuple  # tuple of tuples of Guest, least important first
    default_tier: int  # index into kill_order for guests not listed
    state_file: str
    textfile_dir: str

    @property
    def psi_trigger(self):
        return "%s %d %d" % (
            self.psi_trigger_kind,
            self.psi_trigger_stall_us,
            self.psi_trigger_window_us,
        )

    def tier_of(self, guest):
        for index, tier in enumerate(self.kill_order):
            if guest in tier:
                return index
        return self.default_tier


_INT_KEYS = (
    "psi_trigger_stall_us",
    "psi_trigger_window_us",
    "headroom_threshold_bytes",
    "settle_seconds",
    "exit_grace_seconds",
    "restart_delay_seconds",
    "restart_retry_seconds",
    "restart_min_headroom_bytes",
    "kill_cap",
    "kill_cap_window_seconds",
)


def _parse_kill_order(tiers):
    if not isinstance(tiers, list) or not tiers:
        raise ValueError("kill_order must be a non-empty list of tiers")
    parsed = []
    default_tier = None
    seen = set()
    for index, tier in enumerate(tiers):
        if not isinstance(tier, list) or not tier:
            raise ValueError("kill_order tier %d must be a non-empty list" % index)
        guests = []
        for ref in tier:
            if not isinstance(ref, str):
                raise ValueError("kill_order tier %d: %r is not a string" % (index, ref))
            if ref == DEFAULT_TIER_MARKER:
                if default_tier is not None:
                    raise ValueError("kill_order: %r appears twice" % DEFAULT_TIER_MARKER)
                default_tier = index
                continue
            guest = Guest.parse(ref)
            if guest in seen:
                raise ValueError("kill_order: %s appears twice" % guest)
            seen.add(guest)
            guests.append(guest)
        parsed.append(tuple(guests))
    if default_tier is None:
        raise ValueError("kill_order: no tier contains %r" % DEFAULT_TIER_MARKER)
    return tuple(parsed), default_tier


def load_config(data):
    """Config from a dict (the JSON config file); missing keys take defaults."""
    if not isinstance(data, dict):
        raise ValueError("config must be a JSON object")
    unknown = sorted(set(data) - set(DEFAULT_CONFIG))
    if unknown:
        raise ValueError("unknown config keys: %s" % ", ".join(unknown))
    merged = dict(DEFAULT_CONFIG, **data)
    if not isinstance(merged["dry_run"], bool):
        raise ValueError("dry_run must be true or false")
    for key in _INT_KEYS:
        value = merged[key]
        if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
            raise ValueError("%s must be a positive integer" % key)
    avg10 = merged["restart_max_some_avg10"]
    if isinstance(avg10, bool) or not isinstance(avg10, (int, float)) or not 0 <= avg10 <= 100:
        raise ValueError("restart_max_some_avg10 must be a number from 0 to 100")
    if merged["psi_trigger_kind"] not in ("some", "full"):
        raise ValueError("psi_trigger_kind must be some or full")
    # Kernel limits for PSI triggers (Documentation/accounting/psi.rst).
    if not 500_000 <= merged["psi_trigger_window_us"] <= 10_000_000:
        raise ValueError("psi_trigger_window_us must be from 500000 to 10000000")
    if merged["psi_trigger_stall_us"] > merged["psi_trigger_window_us"]:
        raise ValueError("psi_trigger_stall_us must not exceed psi_trigger_window_us")
    for key in ("state_file", "textfile_dir"):
        if not isinstance(merged[key], str) or not merged[key].startswith("/"):
            raise ValueError("%s must be an absolute path" % key)
    kill_order, default_tier = _parse_kill_order(merged.pop("kill_order"))
    merged["restart_max_some_avg10"] = float(avg10)
    return Config(kill_order=kill_order, default_tier=default_tier, **merged)


def load_config_file(path):
    with open(path, encoding="utf-8") as fh:
        return load_config(json.load(fh))


# --- host-wide headroom --------------------------------------------------

def parse_meminfo(text):
    """/proc/meminfo as {key: bytes}; values without a unit stay as is."""
    result = {}
    for line in text.splitlines():
        key, sep, rest = line.partition(":")
        fields = rest.split()
        if not sep or not fields or not fields[0].isdigit():
            continue
        value = int(fields[0])
        if len(fields) > 1 and fields[1] == "kB":
            value *= 1024
        result[key.strip()] = value
    return result


def parse_arcstats(text):
    """/proc/spl/kstat/zfs/arcstats ("name type data" rows) as {name: int}."""
    result = {}
    for line in text.splitlines():
        fields = line.split()
        if len(fields) == 3 and fields[2].isdigit():
            result[fields[0]] = int(fields[2])
    return result


def parse_psi(text):
    """/proc/pressure/memory as {"some": {"avg10": ..., "total": ...}, "full": {...}}."""
    result = {}
    for line in text.splitlines():
        fields = line.split()
        if not fields:
            continue
        values = {}
        for field in fields[1:]:
            name, sep, value = field.partition("=")
            if sep:
                values[name] = float(value)
        result[fields[0]] = values
    return result


def headroom_bytes(meminfo, arcstats):
    """MemAvailable + reclaimable ARC (size above c_min) + SwapFree.

    ARC is neither in MemAvailable nor memcg-charged, but the shrinker gives it
    back down to c_min within seconds, so it counts as available here.
    """
    arc_spare = max(0, arcstats.get("size", 0) - arcstats.get("c_min", 0))
    return meminfo["MemAvailable"] + arc_spare + meminfo.get("SwapFree", 0)


# --- victim selection ------------------------------------------------------

def select_victim(running, config, excluded=frozenset()):
    """The guest to kill next, or None.

    `running` maps each running guest (its cgroup exists) to its
    memory.current. Within a tier the guest using the most memory goes first.
    """
    candidates = [guest for guest in running if guest not in excluded]
    if not candidates:
        return None
    return min(
        candidates,
        key=lambda guest: (config.tier_of(guest), -running[guest], guest),
    )


# --- kill history, restarts and the kill cap -------------------------------

@dataclasses.dataclass
class PendingRestart:
    due: float
    attempts: int = 0


class Ledger:
    """Kill history, pending restarts and held guests; persisted as JSON."""

    def __init__(self):
        self.kills = {}    # Guest -> [kill timestamps], oldest first
        self.pending = {}  # Guest -> PendingRestart
        self.held = set()  # Guests that hit the kill cap and stay stopped

    def record_kill(self, guest, now, config):
        """Record a kill; returns True when the guest is now held stopped."""
        window = config.kill_cap_window_seconds
        recent = [t for t in self.kills.get(guest, []) if now - t < window]
        recent.append(now)
        self.kills[guest] = recent
        if len(recent) >= config.kill_cap:
            self.held.add(guest)
            self.pending.pop(guest, None)
            return True
        self.pending[guest] = PendingRestart(due=now + config.restart_delay_seconds)
        return False

    def last_kill(self, guest):
        times = self.kills.get(guest)
        return times[-1] if times else None

    def kills_within(self, guest, now, seconds):
        return sum(1 for t in self.kills.get(guest, []) if now - t < seconds)

    def due_restarts(self, now):
        return sorted(g for g, p in self.pending.items() if p.due <= now)

    def next_due(self):
        return min((p.due for p in self.pending.values()), default=None)

    def reschedule(self, guest, due, attempted):
        pending = self.pending[guest]
        pending.due = due
        if attempted:
            pending.attempts += 1

    def prune(self, now, window):
        """Drop kill timestamps older than the cap window; True if any went."""
        changed = False
        for guest in list(self.kills):
            recent = [t for t in self.kills[guest] if now - t < window]
            if recent != self.kills[guest]:
                changed = True
                if recent:
                    self.kills[guest] = recent
                else:
                    del self.kills[guest]
        return changed

    def to_json(self):
        return json.dumps(
            {
                "version": STATE_VERSION,
                "kills": {str(g): t for g, t in sorted(self.kills.items())},
                "pending_restarts": {
                    str(g): {"due": p.due, "attempts": p.attempts}
                    for g, p in sorted(self.pending.items())
                },
                "held": [str(g) for g in sorted(self.held)],
            },
            indent=2,
            sort_keys=True,
        ) + "\n"

    @classmethod
    def from_json(cls, text):
        data = json.loads(text)
        if data.get("version") != STATE_VERSION:
            raise ValueError("unknown state version %r" % data.get("version"))
        ledger = cls()
        for ref, times in data.get("kills", {}).items():
            ledger.kills[Guest.parse(ref)] = sorted(float(t) for t in times)
        for ref, pending in data.get("pending_restarts", {}).items():
            ledger.pending[Guest.parse(ref)] = PendingRestart(
                due=float(pending["due"]), attempts=int(pending.get("attempts", 0))
            )
        ledger.held = {Guest.parse(ref) for ref in data.get("held", [])}
        return ledger


# --- the daemon's decisions ----------------------------------------------

def _gib(value):
    return "%.1f GiB" % (value / GIB)


def _escape_label(value):
    return value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")


def _guest_labels(guest):
    return 'type="%s",vmid="%d"' % (guest.kind, guest.vmid)


class Memguard:
    """Decisions of the daemon; all host access goes through `host` and `log`."""

    def __init__(self, config, host, ledger, log):
        self.config = config
        self.host = host
        self.ledger = ledger
        self.log = log
        self.pressure_events = collections.Counter()
        self.kills = collections.Counter()
        self.would_kills = collections.Counter()
        self.starts = collections.Counter()
        self.last_trigger = None
        self.last_headroom = None
        self._metrics_error = None

    def read_headroom(self):
        headroom = headroom_bytes(
            parse_meminfo(self.host.read_meminfo()),
            parse_arcstats(self.host.read_arcstats()),
        )
        self.last_headroom = headroom
        return headroom

    def exiting_guests(self, running, now):
        """Running guests killed less than exit_grace_seconds ago."""
        return {
            guest
            for guest in running
            if self.ledger.kills_within(guest, now, self.config.exit_grace_seconds)
        }

    def handle_pressure_event(self, now):
        """React to one PSI trigger event; returns True if the daemon should settle."""
        cfg = self.config
        self.last_trigger = now
        headroom = self.read_headroom()
        full_avg10 = parse_psi(self.host.read_psi()).get("full", {}).get("avg10", 0.0)
        situation = "memory pressure (full avg10=%.2f), headroom %s, threshold %s" % (
            full_avg10, _gib(headroom), _gib(cfg.headroom_threshold_bytes))
        if headroom >= cfg.headroom_threshold_bytes:
            self.pressure_events["headroom_ok"] += 1
            self.log.info("%s: not host-wide, ignored" % situation)
            return False

        running = self.host.running_guests()
        exiting = self.exiting_guests(running, now)
        # Memory of guests that are still exiting comes back without another
        # kill; count it so a slow exit does not cascade into more kills.
        exiting_bytes = sum(running[guest] for guest in exiting)
        if exiting and headroom + exiting_bytes >= cfg.headroom_threshold_bytes:
            self.pressure_events["awaiting_exit"] += 1
            self.log.info("%s: waiting for %s to exit (%s)" % (
                situation, ", ".join(g.label for g in sorted(exiting)), _gib(exiting_bytes)))
            return True

        victim = select_victim(running, cfg, exiting)
        if victim is None:
            self.pressure_events["no_victim"] += 1
            self.log.event("%s: no guest left to kill" % situation)
            return True
        what = "%s (%s in use)" % (victim.label, _gib(running[victim]))
        if cfg.dry_run:
            self.pressure_events["would_kill"] += 1
            self.would_kills[victim] += 1
            self.log.event("dry-run: %s: would kill %s" % (situation, what))
            return True
        try:
            self.host.kill(victim)
        except OSError as exc:
            self.pressure_events["kill_failed"] += 1
            self.log.event("%s: killing %s failed: %s" % (situation, what, exc))
            return True
        self.pressure_events["kill"] += 1
        self.kills[victim] += 1
        held = self.ledger.record_kill(victim, now, cfg)
        if held:
            self.log.event(
                "%s: killed %s; %d kills within %ds, holding it stopped" % (
                    situation, what, cfg.kill_cap, cfg.kill_cap_window_seconds))
        else:
            self.log.event("%s: killed %s; restart in %ds" % (
                situation, what, cfg.restart_delay_seconds))
        self.save_state()
        return True

    def restart_blocker(self):
        """Why a guest must not be started now, or None when it may."""
        cfg = self.config
        some_avg10 = parse_psi(self.host.read_psi()).get("some", {}).get("avg10", 0.0)
        if some_avg10 > cfg.restart_max_some_avg10:
            return "memory pressure some avg10=%.2f > %.2f" % (
                some_avg10, cfg.restart_max_some_avg10)
        headroom = self.read_headroom()
        if headroom < cfg.restart_min_headroom_bytes:
            return "headroom %s < %s" % (_gib(headroom), _gib(cfg.restart_min_headroom_bytes))
        return None

    def handle_tick(self, now):
        """Release held guests that run again; start guests whose restart is due."""
        cfg = self.config
        ledger = self.ledger
        running = self.host.running_guests()
        changed = False
        for guest in sorted(ledger.held):
            last = ledger.last_kill(guest)
            # Someone started it by hand: it is no longer held stopped.
            if guest in running and (last is None or now - last >= cfg.restart_delay_seconds):
                ledger.held.discard(guest)
                self.log.event("%s is running again, no longer held stopped" % guest.label)
                changed = True
        for guest in ledger.due_restarts(now):
            retry_at = now + cfg.restart_retry_seconds
            changed = True
            if guest in running:
                del ledger.pending[guest]
                self.log.event("%s is running again after its kill" % guest.label)
                continue
            if cfg.dry_run:
                ledger.reschedule(guest, retry_at, attempted=False)
                self.log.info("dry-run: would start %s" % guest.label)
                continue
            blocker = self.restart_blocker()
            if blocker:
                ledger.reschedule(guest, retry_at, attempted=False)
                self.log.info("not starting %s yet: %s; retry in %ds" % (
                    guest.label, blocker, cfg.restart_retry_seconds))
                continue
            ledger.reschedule(guest, retry_at, attempted=True)
            try:
                self.host.start(guest)
            except (OSError, subprocess.SubprocessError) as exc:
                self.log.event("starting %s failed: %s; retry in %ds" % (
                    guest.label, exc, cfg.restart_retry_seconds))
                continue
            self.starts[guest] += 1
            self.log.event("starting %s (attempt %d)" % (
                guest.label, ledger.pending[guest].attempts))
        if ledger.prune(now, cfg.kill_cap_window_seconds):
            changed = True
        if changed:
            self.save_state()

    def poll_timeout(self, now):
        due = self.ledger.next_due()
        if due is None:
            return MAX_POLL_SECONDS
        return min(MAX_POLL_SECONDS, max(1.0, due - now))

    def save_state(self):
        self.host.write_state(self.ledger.to_json())

    def render_metrics(self, now):
        lines = []

        def metric(name, kind, help_text, samples):
            lines.append("# HELP %s %s" % (name, help_text))
            lines.append("# TYPE %s %s" % (name, kind))
            for labels, value in samples:
                lines.append("%s%s %s" % (name, "{%s}" % labels if labels else "", value))

        def per_guest(counter):
            return [(_guest_labels(g), n) for g, n in sorted(counter.items())]

        metric("pve_memguard_dry_run", "gauge",
               "1 while pve-memguard only logs what it would kill.",
               [("", int(self.config.dry_run))])
        metric("pve_memguard_heartbeat_timestamp_seconds", "gauge",
               "Last time the pve-memguard main loop ran.", [("", "%.0f" % now)])
        if self.last_trigger is not None:
            metric("pve_memguard_last_trigger_timestamp_seconds", "gauge",
                   "Last memory PSI trigger event.", [("", "%.0f" % self.last_trigger)])
        if self.last_headroom is not None:
            metric("pve_memguard_headroom_bytes", "gauge",
                   "Host-wide headroom (MemAvailable + ARC above c_min + SwapFree) "
                   "at the last check.",
                   [("", self.last_headroom)])
        metric("pve_memguard_pressure_events_total", "counter",
               "Memory PSI trigger events by the decision taken.",
               [('decision="%s"' % _escape_label(d), self.pressure_events[d])
                for d in PRESSURE_DECISIONS])
        metric("pve_memguard_kills_total", "counter",
               "Guests killed through cgroup.kill.", per_guest(self.kills))
        metric("pve_memguard_would_kills_total", "counter",
               "Guests that would have been killed in dry-run mode.",
               per_guest(self.would_kills))
        metric("pve_memguard_starts_total", "counter",
               "Guest starts launched after a kill.", per_guest(self.starts))
        metric("pve_memguard_restart_pending", "gauge",
               "1 for each killed guest waiting to be started again.",
               [(_guest_labels(g), 1) for g in sorted(self.ledger.pending)])
        metric("pve_memguard_guest_held_stopped", "gauge",
               "1 for each guest that hit the kill cap and stays stopped.",
               [(_guest_labels(g), 1) for g in sorted(self.ledger.held)])
        return "\n".join(lines) + "\n"

    def write_metrics(self, now):
        try:
            self.host.write_metrics(self.render_metrics(now))
        except OSError as exc:
            message = str(exc)
            if message != self._metrics_error:
                self.log.event("writing metrics failed: %s" % message)
            self._metrics_error = message
        else:
            self._metrics_error = None


# --- host access -----------------------------------------------------------

def atomic_write(path, content, mode=0o644):
    directory = os.path.dirname(path) or "."
    fd, tmp = tempfile.mkstemp(dir=directory, prefix=".pve_memguard.", suffix=".tmp")
    try:
        with os.fdopen(fd, "w") as fh:
            fh.write(content)
        os.chmod(tmp, mode)
        os.replace(tmp, path)
    except BaseException:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def _read(path):
    with open(path, encoding="ascii", errors="replace") as fh:
        return fh.read()


class SysHost:
    """The real host: /proc, cgroupfs, systemd-run."""

    # Guest cgroups as PVE creates them: CTs through lxc.cgroup.dir.container,
    # VMs as transient scopes of qemu-server.
    GUEST_CGROUPS = (("ct", "lxc", ""), ("vm", "qemu.slice", ".scope"))
    START_COMMANDS = {"ct": "pct", "vm": "qm"}

    def __init__(self, config, proc_root="/proc", cgroup_root="/sys/fs/cgroup"):
        self.config = config
        self.proc_root = proc_root
        self.cgroup_root = cgroup_root

    def read_meminfo(self):
        return _read(os.path.join(self.proc_root, "meminfo"))

    def read_arcstats(self):
        try:
            return _read(os.path.join(self.proc_root, "spl/kstat/zfs/arcstats"))
        except FileNotFoundError:
            return ""

    def read_psi(self):
        return _read(os.path.join(self.proc_root, "pressure/memory"))

    def guest_cgroup(self, guest):
        for kind, parent, suffix in self.GUEST_CGROUPS:
            if kind == guest.kind:
                return os.path.join(self.cgroup_root, parent, "%d%s" % (guest.vmid, suffix))
        raise ValueError(guest)

    def running_guests(self):
        """{Guest: memory.current} for every guest whose cgroup exists."""
        running = {}
        for kind, parent, suffix in self.GUEST_CGROUPS:
            try:
                names = os.listdir(os.path.join(self.cgroup_root, parent))
            except FileNotFoundError:
                continue
            for name in names:
                if suffix:
                    if not name.endswith(suffix):
                        continue
                    name = name[: -len(suffix)]
                if not (name.isascii() and name.isdigit()):
                    continue
                guest = Guest(kind, int(name))
                path = self.guest_cgroup(guest)
                if not os.path.isdir(path):
                    continue
                try:
                    running[guest] = int(_read(os.path.join(path, "memory.current")))
                except (OSError, ValueError):
                    running[guest] = 0
        return running

    def kill(self, guest):
        with open(os.path.join(self.guest_cgroup(guest), "cgroup.kill"), "w") as fh:
            fh.write("1")

    def start(self, guest):
        # A transient unit, so the start outlives a daemon restart and is not
        # charged to the daemon's cgroup.
        subprocess.run(
            [
                "systemd-run", "--quiet", "--collect", "--no-block",
                "--unit=pve-memguard-start-%s-%d" % (guest.kind, guest.vmid),
                "--description=pve-memguard: start %s after a kill" % guest.label,
                self.START_COMMANDS[guest.kind], "start", str(guest.vmid),
            ],
            check=True,
            timeout=60,
            stdin=subprocess.DEVNULL,
            capture_output=True,
        )

    def read_state(self):
        try:
            return _read(self.config.state_file)
        except FileNotFoundError:
            return None

    def write_state(self, text):
        os.makedirs(os.path.dirname(self.config.state_file), exist_ok=True)
        atomic_write(self.config.state_file, text, mode=0o600)

    def write_metrics(self, text):
        atomic_write(os.path.join(self.config.textfile_dir, METRICS_BASENAME), text)


class Log:
    """Decisions go to the journal and /dev/kmsg, routine notes to the journal."""

    def __init__(self, kmsg_path="/dev/kmsg"):
        self.syslog_prefix = bool(os.environ.get("JOURNAL_STREAM"))
        try:
            self.kmsg = os.open(kmsg_path, os.O_WRONLY)
        except OSError:
            self.kmsg = None

    def _journal(self, level, message):
        prefix = "<%d>" % level if self.syslog_prefix else ""
        print(prefix + message, flush=True)

    def info(self, message):
        self._journal(6, message)

    def event(self, message):
        self._journal(4, message)
        if self.kmsg is not None:
            try:
                os.write(self.kmsg, ("<4>pve-memguard: %s\n" % message).encode())
            except OSError:
                pass


class PsiTrigger:
    def __init__(self, spec, path="/proc/pressure/memory"):
        self.fd = os.open(path, os.O_RDWR | os.O_NONBLOCK)
        # The kernel replaces the last byte written with NUL, so send one.
        os.write(self.fd, spec.encode() + b"\0")
        self.poller = select.poll()
        self.poller.register(self.fd, select.POLLPRI)

    def wait(self, timeout_seconds):
        """True if the trigger fired within the timeout."""
        for _fd, events in self.poller.poll(int(timeout_seconds * 1000)):
            if events & select.POLLERR:
                raise OSError("PSI trigger reported POLLERR")
            if events & select.POLLPRI:
                return True
        return False

    def close(self):
        os.close(self.fd)


def mlockall():
    MCL_CURRENT, MCL_FUTURE = 1, 2
    libc = ctypes.CDLL(None, use_errno=True)
    if libc.mlockall(MCL_CURRENT | MCL_FUTURE) != 0:
        errno = ctypes.get_errno()
        raise OSError(errno, "mlockall: %s" % os.strerror(errno))


def load_ledger(host, log):
    text = host.read_state()
    if text is None:
        return Ledger()
    try:
        return Ledger.from_json(text)
    except (ValueError, KeyError, TypeError) as exc:
        log.event("state file unreadable, starting with empty kill history: %s" % exc)
        return Ledger()


def run(config):
    log = Log()
    host = SysHost(config)
    try:
        mlockall()
    except OSError as exc:
        log.event("running without locked memory: %s" % exc)
    guard = Memguard(config, host, load_ledger(host, log), log)
    log.event(
        "started: dry_run=%s, trigger %r, headroom threshold %s, kill order %s" % (
            config.dry_run, config.psi_trigger, _gib(config.headroom_threshold_bytes),
            " | ".join(
                " ".join([str(g) for g in tier]
                         + ([DEFAULT_TIER_MARKER] if i == config.default_tier else []))
                for i, tier in enumerate(config.kill_order))))
    guard.write_metrics(time.time())
    trigger = None
    while True:
        try:
            if trigger is None:
                trigger = PsiTrigger(config.psi_trigger)
            if trigger.wait(guard.poll_timeout(time.time())):
                if guard.handle_pressure_event(time.time()):
                    guard.write_metrics(time.time())
                    time.sleep(config.settle_seconds)
            guard.handle_tick(time.time())
            guard.write_metrics(time.time())
        except Exception as exc:  # keep guarding; re-arm the trigger
            log.event("error, re-arming: %r" % (exc,))
            if trigger is not None:
                try:
                    trigger.close()
                except OSError:
                    pass
                trigger = None
            time.sleep(5)


USAGE = "usage: pve-memguard [--check-config] CONFIG.json"


def main(argv=None):
    argv = list(sys.argv[1:] if argv is None else argv)
    check_only = argv[:1] == ["--check-config"]
    if check_only:
        argv = argv[1:]
    if len(argv) != 1:
        print(USAGE, file=sys.stderr)
        return 2
    try:
        config = load_config_file(argv[0])
    except (OSError, ValueError) as exc:
        print("pve-memguard: bad config %s: %s" % (argv[0], exc), file=sys.stderr)
        return 1
    if check_only:
        print("pve-memguard: config %s is valid (dry_run=%s)" % (argv[0], config.dry_run))
        return 0
    run(config)
    return 0


if __name__ == "__main__":
    sys.exit(main())
