#!/usr/bin/env python3
"""pve-memguard: kill whole PVE guests in rank order under host-wide memory pressure.

Run as `python3 -I`.

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
- In dry-run mode it logs what it would kill and kills nothing. Restarts that
  are already pending still happen.
- State and metrics files are written by a separate thread, so a filesystem
  that blocks under memory pressure never delays a kill.
"""
from __future__ import annotations

import collections
import ctypes
import json
import os
import select
import subprocess
import sys
import tempfile
import threading
import time

GIB = 1 << 30

# Marks the tier in `kill_order` that also takes every running guest that
# is not listed anywhere.
DEFAULT_TIER_MARKER = "default"

# The only place defaults live; the config file overrides single keys.
DEFAULT_CONFIG = {
    "dry_run": True,
    # At least 1 s of "full" stall within 10 s.
    "psi_trigger": "full 1000000 10000000",
    "headroom_threshold_bytes": 3 * GIB,
    # After a kill: wait this long before reacting to the next pressure event.
    "settle_seconds": 15,
    # A killed guest whose cgroup is still there this long after the kill
    # counts as exiting: its memory counts as headroom.
    "exit_grace_seconds": 120,
    # Start a killed guest this long after the kill, then retry this often
    # while memory "some" avg10 is above the limit or headroom below the minimum.
    "restart_delay_seconds": 600,
    "restart_retry_seconds": 300,
    "restart_max_some_avg10": 5.0,
    "restart_min_headroom_bytes": 6 * GIB,
    # The kill_cap-th kill of one guest within the window holds it stopped.
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
}

PRESSURE_DECISIONS = (
    "headroom_ok",    # pressure, but the host as a whole has headroom
    "awaiting_exit",  # headroom will come back once killed guests finish exiting
    "kill",
    "would_kill",     # dry run
    "kill_failed",
    "no_victim",      # nothing left to kill
)

STATE_FILE = "/var/lib/pve-memguard/state.json"
METRICS_FILE = "/var/lib/node_exporter/pve_memguard.prom"
MAX_POLL_SECONDS = 60
ERROR_PAUSE_SECONDS = 5
# mlockall(MCL_FUTURE) locks every thread stack in full; the writer thread
# needs very little.
WRITER_STACK_BYTES = 256 << 10


class Guest(collections.namedtuple("Guest", "kind vmid")):
    """A CT ("ct") or VM ("vm") by its vmid."""

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


class Config:
    """DEFAULT_CONFIG with the keys of `overrides` replaced.

    An unknown key or a value of the wrong type is a ValueError.
    """

    def __init__(self, overrides):
        if not isinstance(overrides, dict):
            raise ValueError("config must be a JSON object")
        for key, value in overrides.items():
            if key not in DEFAULT_CONFIG:
                raise ValueError("unknown config key %r" % key)
            expected = type(DEFAULT_CONFIG[key])
            if type(value) is not expected and not (expected is float and type(value) is int):
                raise ValueError("%s must be of type %s" % (key, expected.__name__))
        vars(self).update(DEFAULT_CONFIG, **overrides)
        self.restart_max_some_avg10 = float(self.restart_max_some_avg10)
        self.kill_order, self.default_tier = _parse_kill_order(self.kill_order)

    def tier_of(self, guest):
        for index, tier in enumerate(self.kill_order):
            if guest in tier:
                return index
        return self.default_tier


def _parse_kill_order(tiers):
    """Tuple of tuples of Guest, and the index of the default tier."""
    parsed = []
    default_tier = None
    for index, tier in enumerate(tiers):
        if not isinstance(tier, list) or not all(isinstance(ref, str) for ref in tier):
            raise ValueError("kill_order tier %d must be a list of strings" % index)
        if DEFAULT_TIER_MARKER in tier:
            if default_tier is not None:
                raise ValueError("kill_order: %r appears twice" % DEFAULT_TIER_MARKER)
            default_tier = index
        parsed.append(tuple(Guest.parse(ref) for ref in tier if ref != DEFAULT_TIER_MARKER))
    if default_tier is None:
        raise ValueError("kill_order: no tier contains %r" % DEFAULT_TIER_MARKER)
    return tuple(parsed), default_tier


def load_config_file(path):
    with open(path, encoding="utf-8") as fh:
        return Config(json.load(fh))


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
        if fields:
            result[fields[0]] = {
                name: float(value)
                for name, sep, value in (field.partition("=") for field in fields[1:])
                if sep
            }
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

class Ledger:
    """Kill history, pending restarts and held guests; persisted as JSON."""

    def __init__(self):
        self.kills = {}    # Guest -> [kill timestamps], oldest first
        self.pending = {}  # Guest -> when its restart is due
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
        self.pending[guest] = now + config.restart_delay_seconds
        return False

    def last_kill(self, guest):
        times = self.kills.get(guest)
        return times[-1] if times else None

    def killed_within(self, guest, now, seconds):
        last = self.last_kill(guest)
        return last is not None and now - last < seconds

    def due_restarts(self, now):
        return [g for g, due in self.pending.items() if due <= now]

    def next_due(self):
        return min(self.pending.values(), default=None)

    def to_json(self):
        return json.dumps(
            {
                "kills": {str(g): t for g, t in self.kills.items()},
                "pending_restarts": {str(g): due for g, due in self.pending.items()},
                "held": [str(g) for g in sorted(self.held)],
            },
            indent=2,
            sort_keys=True,
        ) + "\n"

    @classmethod
    def from_json(cls, text):
        data = json.loads(text)
        ledger = cls()
        for ref, times in data["kills"].items():
            ledger.kills[Guest.parse(ref)] = sorted(float(t) for t in times)
        for ref, due in data["pending_restarts"].items():
            ledger.pending[Guest.parse(ref)] = float(due)
        ledger.held = {Guest.parse(ref) for ref in data["held"]}
        return ledger


# --- writing files off the kill path -----------------------------------------

class SnapshotWriter:
    """Writes the latest state and metrics snapshots from its own thread.

    `submit` only swaps a string in under a lock that is never held during
    I/O, so the main loop never waits for a write. Older snapshots that were
    not written yet are dropped.
    """

    def __init__(self, host, log):
        self.targets = {"state": host.write_state, "metrics": host.write_metrics}
        self.log = log
        self._cond = threading.Condition()
        self._pending = {}  # target name -> text
        self._errors = {}   # target name -> last error message

    def submit(self, target, text):
        with self._cond:
            self._pending[target] = text
            self._cond.notify()

    def flush(self):
        """Write what is pending now, state first; does not wait for more."""
        with self._cond:
            pending, self._pending = self._pending, {}
        for target, write in self.targets.items():
            if target not in pending:
                continue
            try:
                write(pending[target])
            except Exception as exc:  # keep the writer thread alive
                message = str(exc)
                if self._errors.get(target) != message:
                    self.log.event("writing %s failed: %s" % (target, message))
                self._errors[target] = message
            else:
                self._errors.pop(target, None)

    def run(self):
        while True:
            with self._cond:
                self._cond.wait_for(lambda: self._pending)
            self.flush()

    def start(self):
        threading.stack_size(WRITER_STACK_BYTES)
        threading.Thread(target=self.run, name="pve-memguard-writer", daemon=True).start()


# --- the daemon's decisions ----------------------------------------------

def _gib(value):
    return "%.1f GiB" % (value / GIB)


def _guest_labels(guest):
    return 'type="%s",vmid="%d"' % (guest.kind, guest.vmid)


class Memguard:
    """Decisions of the daemon; all host access goes through `host`, `log` and `writer`."""

    def __init__(self, config, host, ledger, log, writer):
        self.config = config
        self.host = host
        self.ledger = ledger
        self.log = log
        self.writer = writer
        self.pressure_events = collections.Counter()
        self.last_headroom = None

    def read_headroom(self):
        headroom = headroom_bytes(
            parse_meminfo(self.host.read_meminfo()),
            parse_arcstats(self.host.read_arcstats()),
        )
        self.last_headroom = headroom
        return headroom

    def handle_pressure_event(self, now):
        """React to one PSI trigger event; returns True if the daemon should settle."""
        cfg = self.config
        ledger = self.ledger
        headroom = self.read_headroom()
        full_avg10 = parse_psi(self.host.read_psi()).get("full", {}).get("avg10", 0.0)
        situation = "memory pressure (full avg10=%.2f), headroom %s, threshold %s" % (
            full_avg10, _gib(headroom), _gib(cfg.headroom_threshold_bytes))
        if headroom >= cfg.headroom_threshold_bytes:
            self.pressure_events["headroom_ok"] += 1
            self.log.info("%s: not host-wide, ignored" % situation)
            return False

        running = self.host.running_guests()
        # A guest that still runs within the restart delay after its kill is
        # stuck exiting (or was started by hand): killing it again does not
        # help, so it is never picked again. Its memory counts as coming back
        # only within the exit grace.
        recent = {g for g in running if ledger.killed_within(g, now, cfg.restart_delay_seconds)}
        exiting = {g for g in recent if ledger.killed_within(g, now, cfg.exit_grace_seconds)}
        exiting_bytes = sum(running[guest] for guest in exiting)
        if exiting and headroom + exiting_bytes >= cfg.headroom_threshold_bytes:
            self.pressure_events["awaiting_exit"] += 1
            self.log.info("%s: waiting for %s to exit (%s)" % (
                situation, ", ".join(g.label for g in sorted(exiting)), _gib(exiting_bytes)))
            return True

        victim = select_victim(running, cfg, recent | ledger.held)
        if victim is None:
            self.pressure_events["no_victim"] += 1
            self.log.event("%s: no guest left to kill" % situation)
            return True
        what = "%s (%s in use)" % (victim.label, _gib(running[victim]))
        if cfg.dry_run:
            self.pressure_events["would_kill"] += 1
            self.log.event("dry-run: %s: would kill %s" % (situation, what))
            return True
        try:
            self.host.kill(victim)
        except OSError as exc:
            self.pressure_events["kill_failed"] += 1
            self.log.event("%s: killing %s failed: %s" % (situation, what, exc))
            return True
        self.pressure_events["kill"] += 1
        if ledger.record_kill(victim, now, cfg):
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
        """Release held guests that run again; start at most one due guest.

        Due guests start most important first. After one start the others
        wait for the next retry, so the gate sees what the started guest uses.
        """
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
        started = None
        retry_at = now + cfg.restart_retry_seconds
        for guest in sorted(ledger.due_restarts(now), key=lambda g: (-cfg.tier_of(g), g)):
            changed = True
            if guest in running:
                del ledger.pending[guest]
                self.log.event("%s is running again after its kill" % guest.label)
                continue
            ledger.pending[guest] = retry_at
            if started is not None:
                self.log.info("not starting %s yet: %s started first; retry in %ds" % (
                    guest.label, started.label, cfg.restart_retry_seconds))
                continue
            blocker = self.restart_blocker()
            if blocker:
                self.log.info("not starting %s yet: %s; retry in %ds" % (
                    guest.label, blocker, cfg.restart_retry_seconds))
                continue
            try:
                self.host.start(guest)
            except (OSError, subprocess.SubprocessError) as exc:
                self.log.event("starting %s failed: %s; retry in %ds" % (
                    guest.label, exc, cfg.restart_retry_seconds))
                continue
            started = guest
            self.log.event("starting %s" % guest.label)
        if changed:
            self.save_state()

    def poll_timeout(self, now):
        due = self.ledger.next_due()
        if due is None:
            return MAX_POLL_SECONDS
        return min(MAX_POLL_SECONDS, max(1.0, due - now))

    def save_state(self):
        self.writer.submit("state", self.ledger.to_json())

    def render_metrics(self, now):
        lines = []

        def metric(name, kind, help_text, samples):
            lines.append("# HELP %s %s" % (name, help_text))
            lines.append("# TYPE %s %s" % (name, kind))
            for labels, value in samples:
                lines.append("%s%s %s" % (name, "{%s}" % labels if labels else "", value))

        metric("pve_memguard_dry_run", "gauge",
               "1 while pve-memguard only logs what it would kill.",
               [("", int(self.config.dry_run))])
        metric("pve_memguard_heartbeat_timestamp_seconds", "gauge",
               "Last time the pve-memguard main loop ran.", [("", "%.0f" % now)])
        if self.last_headroom is not None:
            metric("pve_memguard_headroom_bytes", "gauge",
                   "Host-wide headroom (MemAvailable + ARC above c_min + SwapFree) "
                   "at the last check.",
                   [("", self.last_headroom)])
        metric("pve_memguard_pressure_events_total", "counter",
               "Memory PSI trigger events by the decision taken.",
               [('decision="%s"' % d, self.pressure_events[d]) for d in PRESSURE_DECISIONS])
        metric("pve_memguard_restart_pending", "gauge",
               "1 for each killed guest waiting to be started again.",
               [(_guest_labels(g), 1) for g in sorted(self.ledger.pending)])
        metric("pve_memguard_guest_held_stopped", "gauge",
               "1 for each guest that hit the kill cap and stays stopped.",
               [(_guest_labels(g), 1) for g in sorted(self.ledger.held)])
        return "\n".join(lines) + "\n"

    def publish_metrics(self, now):
        self.writer.submit("metrics", self.render_metrics(now))


def loop_once(guard, trigger, open_trigger, clock=time.time, sleep=time.sleep):
    """One pass of the main loop; returns the trigger for the next pass.

    On any error the trigger is closed and None returned, so the next pass
    opens a new one (a trigger fd cannot be armed twice).
    """
    try:
        if trigger is None:
            trigger = open_trigger()
        if trigger.wait(guard.poll_timeout(clock())):
            if guard.handle_pressure_event(clock()):
                guard.publish_metrics(clock())
                sleep(guard.config.settle_seconds)
        guard.handle_tick(clock())
        guard.publish_metrics(clock())
        return trigger
    except Exception as exc:  # keep guarding
        guard.log.event("error, re-arming: %r" % (exc,))
        if trigger is not None:
            try:
                trigger.close()
            except OSError:
                pass
        sleep(ERROR_PAUSE_SECONDS)
        return None


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

    def __init__(self, proc_root="/proc", cgroup_root="/sys/fs/cgroup",
                 state_file=STATE_FILE, metrics_file=METRICS_FILE):
        self.proc_root = proc_root
        self.cgroup_root = cgroup_root
        self.state_file = state_file
        self.metrics_file = metrics_file

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
            return _read(self.state_file)
        except FileNotFoundError:
            return None

    def write_state(self, text):
        os.makedirs(os.path.dirname(self.state_file), exist_ok=True)
        atomic_write(self.state_file, text, mode=0o600)

    def write_metrics(self, text):
        atomic_write(self.metrics_file, text)


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
    except (ValueError, KeyError, TypeError, AttributeError) as exc:
        log.event("state file unreadable, starting with empty kill history: %r" % (exc,))
        return Ledger()


def run(config):
    log = Log()
    host = SysHost()
    try:
        mlockall()
    except OSError as exc:
        log.event("running without locked memory: %s" % exc)
    writer = SnapshotWriter(host, log)
    writer.start()
    guard = Memguard(config, host, load_ledger(host, log), log, writer)
    log.event("started: dry_run=%s, trigger %r, headroom threshold %s" % (
        config.dry_run, config.psi_trigger, _gib(config.headroom_threshold_bytes)))
    guard.publish_metrics(time.time())
    trigger = None
    while True:
        trigger = loop_once(guard, trigger, lambda: PsiTrigger(config.psi_trigger))


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
    if not check_only:
        run(config)
    return 0


if __name__ == "__main__":
    sys.exit(main())
