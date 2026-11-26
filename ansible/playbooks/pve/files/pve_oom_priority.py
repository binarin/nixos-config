#!/usr/bin/python3 -I
"""pve-oom-priority: per-guest oom_score_adj from pve-memguard's kill order.

Run as `python3 -I`.

The kill order is not repeated here: the tool imports the deployed daemon
(/usr/local/sbin/pve-memguard) and builds the effective config with the
daemon's own Config from /etc/pve-memguard/config.json (a missing file means
the daemon's defaults).

Mapping. kill_order lists tiers least important first; d is the index of the
default tier and n_below the number of tiers after it.
- A tier i before d gets round(1000 * (d - i) / d): the first tier is +1000.
- The default tier, and every guest that is not listed, gets 0.
- A tier j after d gets round(-750 * (j - d) / n_below): the last is -750.
-750 stays above the -900 of the host services (pve-cluster, corosync)
and never reaches -1000 (unkillable), so the most important guest stays
killable as a last resort. round() is Python's (ties to even).

Floors. A write to /proc/<pid>/oom_score_adj by a process with effective
CAP_SYS_RESOURCE (host root) also sets the process's oom_score_adj_min, which
its children inherit; below it, a process without that capability (anything
in an unprivileged CT) gets EACCES. Container engines write the OCI spec's
oom_score_adj (0 by default) and fail on EACCES (runc, crun), so a positive
floor would break Docker and podman inside a CT. A negative value is written
with a floor, a value >= 0 without one (CAP_SYS_RESOURCE is cleared from the
effective set for the write): write_oom_score_adj(pid, value, floor) with
floor = value < 0, used by every command. Without a floor, processes in a CT
with a positive value can set themselves lower again (an engine's 0, user@'s
+100); `apply` raises them when it runs.

How the value reaches a guest:
- CT: `sync` writes /etc/lxc/oom-priority/<id>.conf, pulled in by
  `lxc.include:` in /etc/pve/lxc/<id>.conf (`include`, a hand step):
  - value < 0: `lxc.proc.oom_score_adj = N`. lxc writes it as host root
    before the CT init runs, so it is also the floor of the whole CT.
  - value 0: only the comment header.
  - value > 0: `lxc.hook.start-host = <LXC_HOOK>`. lxc runs that wrapper
    in the host namespaces before it wakes the CT init, with the init's PID
    in LXC_PID; it calls `lxc-start-host`, which writes N without a floor.
    The wrapper always exits 0, so it never blocks a start.
- VM: the hookscript snippet calls `hook <vmid> post-start`, which writes the
  value to the kvm process.
- Running guests: `apply` writes the value to their processes now.

Commands:
  values                print guest, tier and value
  sync [--check]        write the include file of every CT
  include [--check] VMID...
                        add the lxc.include line to CT configs
  apply [--check]       set the value on the processes of running guests
  check                 verify all of the above; exit 1 on a problem
  hook VMID PHASE       qm hookscript entry; always exits 0
  lxc-start-host [NAME lxc start-host]
                        lxc.hook.start-host entry; always exits 0
With --check nothing is changed and the output says what would be. sync,
include and apply end with a line "changed: N" (with --check: would change).
"""
from __future__ import annotations

import argparse
import contextlib
import ctypes
import difflib
import errno
import fcntl
import importlib.machinery
import importlib.util
import os
import re
import sys
import time

MEMGUARD_PATH = "/usr/local/sbin/pve-memguard"
MEMGUARD_CONFIG = "/etc/pve-memguard/config.json"
PVE_LXC_DIR = "/etc/pve/lxc"
PVE_QEMU_DIR = "/etc/pve/qemu-server"
INCLUDE_DIR = "/etc/lxc/oom-priority"
CGROUP_ROOT = "/sys/fs/cgroup"
QEMU_PID_DIR = "/var/run/qemu-server"
PROC = "/proc"
# pve-container's config lock (PVE::LXC::Config->config_file_lock).
LXC_CONFIG_LOCK = "/run/lock/lxc/pve-config-%d.lock"
LOCK_TIMEOUT_SECONDS = 10
HOOKSCRIPT = "local:snippets/oom-priority-hook.sh"
HOOKSCRIPT_FILE = "/var/lib/vz/snippets/oom-priority-hook.sh"
# lxc.hook.start-host wrapper for CTs with a positive value.
LXC_HOOK = "/usr/local/libexec/pve-oom-priority-lxc-hook"

MOST_UNIMPORTANT_VALUE = 1000
MOST_IMPORTANT_VALUE = -750

CONF_NAME = re.compile(r"^([0-9]+)\.conf$")
# A raw lxc key in a pct config, `key: value` or `key = value`
# (PVE::LXC::Config::parse_pct_config).
LXC_KEY_LINE = re.compile(r"^(lxc\.[a-z0-9_\-\.]+)(:|\s*=)\s*(.*?)\s*$")

CAP_SYS_RESOURCE = 24
LINUX_CAPABILITY_VERSION_3 = 0x20080522


class _CapHeader(ctypes.Structure):
    _fields_ = [("version", ctypes.c_uint32), ("pid", ctypes.c_int)]


class _CapData(ctypes.Structure):
    _fields_ = [("effective", ctypes.c_uint32), ("permitted", ctypes.c_uint32),
                ("inheritable", ctypes.c_uint32)]


@contextlib.contextmanager
def without_sys_resource():
    """CAP_SYS_RESOURCE cleared from this thread's effective set, then restored.

    It stays in the permitted set, so it can be restored; CAP_DAC_OVERRIDE,
    needed to write /proc files of CT processes, is untouched.
    """
    libc = ctypes.CDLL(None, use_errno=True)
    header = _CapHeader(LINUX_CAPABILITY_VERSION_3, 0)
    data = (_CapData * 2)()

    def call(function, what):
        if function(ctypes.byref(header), data) != 0:
            code = ctypes.get_errno()
            raise OSError(code, "%s: %s" % (what, os.strerror(code)))

    call(libc.capget, "capget")
    saved = data[0].effective
    data[0].effective = saved & ~(1 << CAP_SYS_RESOURCE)
    call(libc.capset, "capset")
    try:
        yield
    finally:
        data[0].effective = saved
        call(libc.capset, "capset (restore)")


def load_memguard(path=MEMGUARD_PATH):
    """The pve-memguard daemon as a module; importing it has no side effects."""
    loader = importlib.machinery.SourceFileLoader("pve_memguard", path)
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    # No __pycache__ next to the deployed daemon.
    saved, sys.dont_write_bytecode = sys.dont_write_bytecode, True
    try:
        loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = saved
    return module


def tier_values(tier_count, default_tier):
    """oom_score_adj for each kill_order tier index (see the module docstring)."""
    below = tier_count - 1 - default_tier
    values = []
    for index in range(tier_count):
        if index < default_tier:
            values.append(round(MOST_UNIMPORTANT_VALUE * (default_tier - index) / default_tier))
        elif index == default_tier:
            values.append(0)
        else:
            values.append(round(MOST_IMPORTANT_VALUE * (index - default_tier) / below))
    return values


class Priorities:
    """oom_score_adj per guest for one memguard Config."""

    def __init__(self, config):
        self.config = config
        self.values = tier_values(len(config.kill_order), config.default_tier)

    def tier_of(self, guest):
        return self.config.tier_of(guest)

    def value_of(self, guest):
        return self.values[self.tier_of(guest)]


def settled(current, target):
    """True if `apply` leaves a process at `current` alone.

    A process left at 0 or below the target gets the target; one that raised
    itself above the target (Docker, chromium at 300) keeps its value.
    Limitation: 0 is taken as "never set", because /proc does not show
    oom_score_adj_min. A process that sets itself to 0 on purpose in a CT
    with a negative value (allowed, 0 is above the floor) is set back to the
    target by every `apply`, which then always reports a change.
    """
    return current == target or (current > target and current != 0)


def include_path(vmid):
    return "%s/%d.conf" % (INCLUDE_DIR, vmid)


def include_line(vmid):
    return "lxc.include: %s\n" % include_path(vmid)


def include_file_text(vmid, tier, value):
    """The include file for a CT (see the module docstring)."""
    text = (
        "# Written by pve-oom-priority sync from pve-memguard's kill_order;\n"
        "# do not edit. CT %d: tier %d, oom_score_adj %d.\n" % (vmid, tier, value)
    )
    if value < 0:
        return text + "lxc.proc.oom_score_adj = %d\n" % value
    if value > 0:
        return text + (
            "# Positive: set without a floor by the hook, so container engines\n"
            "# in the CT can still write 0.\n"
            "lxc.hook.start-host = %s\n" % LXC_HOOK)
    return text


def main_section_end(lines):
    """Index of the first section header ([snapshot], [pve:pending])."""
    for index, line in enumerate(lines):
        if line.startswith("["):
            return index
    return len(lines)


def has_include(lines, vmid):
    """True if the main section has the lxc.include line for this CT."""
    for line in lines[:main_section_end(lines)]:
        match = LXC_KEY_LINE.match(line.rstrip("\n"))
        if match and match.group(1) == "lxc.include" and match.group(3) == include_path(vmid):
            return True
    return False


def with_include(text, vmid):
    """The CT config text with the include line at the end of the main section."""
    lines = text.splitlines(keepends=True)
    if has_include(lines, vmid):
        return text
    end = main_section_end(lines)
    while end > 0 and not lines[end - 1].strip():
        end -= 1
    if end > 0 and not lines[end - 1].endswith("\n"):
        lines[end - 1] += "\n"
    lines.insert(end, include_line(vmid))
    return "".join(lines)


def config_value(text, key):
    """The value of `key:` in the main section of a PVE guest config, or None."""
    lines = text.splitlines()
    for line in lines[:main_section_end(lines)]:
        name, sep, value = line.partition(":")
        if sep and name.strip() == key:
            return value.strip()
    return None


class Host:
    """Files of the PVE host under `root` (tests use a temp dir).

    `without_floor` is the context manager that drops CAP_SYS_RESOURCE.
    """

    def __init__(self, root="/", without_floor=without_sys_resource):
        self.root = root
        self.without_floor = without_floor

    def path(self, path):
        return os.path.join(self.root, path.lstrip("/"))

    def read(self, path):
        with open(self.path(path), encoding="utf-8") as fh:
            return fh.read()

    def read_or_none(self, path):
        """None if the file, or the process whose /proc file it is, is gone."""
        try:
            return self.read(path)
        except (FileNotFoundError, ProcessLookupError):
            return None

    def exists(self, path):
        return os.path.exists(self.path(path))

    def guest_ids(self, directory):
        try:
            names = os.listdir(self.path(directory))
        except FileNotFoundError:
            return []
        return sorted(int(match.group(1)) for match in map(CONF_NAME.match, names) if match)

    def ct_ids(self):
        return self.guest_ids(PVE_LXC_DIR)

    def vm_ids(self):
        return self.guest_ids(PVE_QEMU_DIR)

    def write_atomically(self, path, text, mode, fsync):
        """Write through a temp file in the same directory, then rename.

        The temp name follows PVE's file_set_contents, which writes
        /etc/pve (pmxcfs) the same way.
        """
        target = self.path(path)
        temp = "%s.tmp.%d" % (target, os.getpid())
        fd = os.open(temp, os.O_WRONLY | os.O_CREAT | os.O_EXCL, mode)
        try:
            with os.fdopen(fd, "w", encoding="utf-8") as fh:
                fh.write(text)
                if fsync:
                    fh.flush()
                    os.fsync(fh.fileno())
            os.rename(temp, target)
        except BaseException:
            try:
                os.unlink(temp)
            except FileNotFoundError:
                pass
            raise

    def lock_ct_config(self, vmid):
        """Hold pve-container's lock on the CT config (close the fd to release)."""
        path = self.path(LXC_CONFIG_LOCK % vmid)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        fd = os.open(path, os.O_RDWR | os.O_CREAT, 0o644)
        deadline = time.monotonic() + LOCK_TIMEOUT_SECONDS
        while True:
            try:
                fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                return fd
            except BlockingIOError:
                if time.monotonic() > deadline:
                    os.close(fd)
                    raise OSError(errno.EAGAIN, "CT %d config is locked" % vmid)
                time.sleep(0.1)

    # --- processes -------------------------------------------------------

    def ct_pids(self, vmid):
        """PIDs in the CT's cgroup tree, or None if the CT is not running."""
        top = self.path("%s/lxc/%d" % (CGROUP_ROOT, vmid))
        if not os.path.isdir(top):
            return None
        pids = set()
        for directory, _dirs, files in os.walk(top):
            if "cgroup.procs" in files:
                try:
                    with open(os.path.join(directory, "cgroup.procs"), encoding="ascii") as fh:
                        pids.update(int(line) for line in fh if line.strip())
                except FileNotFoundError:
                    pass  # a cgroup removed while walking
        return sorted(pids)

    def vm_pid(self, vmid):
        """The kvm PID, or None if the VM is not running."""
        text = self.read_or_none("%s/%d.pid" % (QEMU_PID_DIR, vmid))
        if text is None or not text.strip().isdigit():
            return None
        pid = int(text.strip())
        # A stale pid file may name a reused PID: it must be in the VM's scope.
        cgroup = self.read_or_none("%s/%d/cgroup" % (PROC, pid))
        if cgroup is None or not any(
                line.endswith("/qemu.slice/%d.scope" % vmid) for line in cgroup.splitlines()):
            return None
        return pid

    def is_namespace_init(self, pid):
        """True if the process is PID 1 of a PID namespace one below the host's.

        That is the CT init; init processes of containers nested in the CT
        have more than two PIDs in NSpid.
        """
        status = self.read_or_none("%s/%d/status" % (PROC, pid))
        for line in (status or "").splitlines():
            if line.startswith("NSpid:"):
                fields = line.split()
                return len(fields) == 3 and fields[2] == "1"
        return False

    def oom_score_adj(self, pid):
        """The process's oom_score_adj, or None if it is gone."""
        text = self.read_or_none("%s/%d/oom_score_adj" % (PROC, pid))
        return None if text is None else int(text)

    def write_oom_score_adj(self, pid, value, floor):
        """Set the process's oom_score_adj; False if the process is gone.

        With `floor` the value also becomes its oom_score_adj_min. Without,
        the kernel refuses a value below the process's current minimum
        (EACCES); then the value is written with a floor, which lowers that
        minimum to the value, the closest the kernel allows.
        """
        if floor:
            return self._write_oom_score_adj(pid, value)
        try:
            with self.without_floor():
                return self._write_oom_score_adj(pid, value)
        except PermissionError:
            return self._write_oom_score_adj(pid, value)

    def _write_oom_score_adj(self, pid, value):
        try:
            fd = os.open(self.path("%s/%d/oom_score_adj" % (PROC, pid)), os.O_WRONLY | os.O_TRUNC)
            try:
                os.write(fd, b"%d\n" % value)
            finally:
                os.close(fd)
        except OSError as exc:
            if exc.errno in (errno.ENOENT, errno.ESRCH):
                return False
            raise
        return True


def load_priorities(memguard, host):
    path = host.path(MEMGUARD_CONFIG)
    if os.path.exists(path):
        config = memguard.load_config_file(path)
    else:
        config = memguard.Config({})
    return Priorities(config)


def host_guests(memguard, host):
    return [memguard.Guest("ct", vmid) for vmid in host.ct_ids()] + [
        memguard.Guest("vm", vmid) for vmid in host.vm_ids()]


# --- commands ------------------------------------------------------------

def cmd_values(memguard, host, out):
    priorities = load_priorities(memguard, host)
    listed = [guest for tier in priorities.config.kill_order for guest in tier]
    guests = sorted(set(listed) | set(host_guests(memguard, host)),
                    key=lambda guest: (priorities.tier_of(guest), guest))
    print("%-8s %4s %13s" % ("GUEST", "TIER", "OOM_SCORE_ADJ"), file=out)
    for guest in guests:
        print("%-8s %4d %13d" % (guest, priorities.tier_of(guest), priorities.value_of(guest)), file=out)
    print("%-8s %4d %13d" % ("unlisted", priorities.config.default_tier,
                             priorities.values[priorities.config.default_tier]), file=out)
    return 0


def cmd_sync(memguard, host, check, out):
    priorities = load_priorities(memguard, host)
    verb = "would write" if check else "wrote"
    changed = 0
    for vmid in host.ct_ids():
        guest = memguard.Guest("ct", vmid)
        text = include_file_text(vmid, priorities.tier_of(guest), priorities.value_of(guest))
        if host.read_or_none(include_path(vmid)) == text:
            continue
        if not check:
            os.makedirs(host.path(INCLUDE_DIR), mode=0o755, exist_ok=True)
            host.write_atomically(include_path(vmid), text, 0o644, fsync=True)
        print("%s %s (oom_score_adj %d)" % (verb, include_path(vmid), priorities.value_of(guest)), file=out)
        changed += 1
    print("changed: %d" % changed, file=out)
    return 0


def cmd_include(host, vmids, check, out, err):
    problems = []
    for vmid in vmids:
        text = host.read_or_none("%s/%d.conf" % (PVE_LXC_DIR, vmid))
        if text is None:
            problems.append("CT %d: no %s/%d.conf" % (vmid, PVE_LXC_DIR, vmid))
        elif config_value(text, "lock"):
            problems.append("CT %d is locked (lock: %s)" % (vmid, config_value(text, "lock")))
        elif not host.exists(include_path(vmid)):
            problems.append("CT %d: %s does not exist; run `pve-oom-priority sync` first"
                            % (vmid, include_path(vmid)))
    if problems:
        for problem in problems:
            print("refused: %s" % problem, file=err)
        print("nothing changed", file=err)
        return 1
    changed = 0
    code = 0
    for vmid in vmids:
        path = "%s/%d.conf" % (PVE_LXC_DIR, vmid)
        lock = None if check else host.lock_ct_config(vmid)
        try:
            text = host.read(path)
            if config_value(text, "lock"):
                print("skipped: CT %d was locked meanwhile (lock: %s)"
                      % (vmid, config_value(text, "lock")), file=err)
                code = 1
                continue
            new = with_include(text, vmid)
            if new == text:
                print("CT %d: already includes %s" % (vmid, include_path(vmid)), file=out)
                continue
            out.writelines(difflib.unified_diff(
                text.splitlines(keepends=True), new.splitlines(keepends=True), path, path))
            if not check:
                host.write_atomically(path, new, 0o640, fsync=False)
            changed += 1
        finally:
            if lock is not None:
                os.close(lock)
    print("changed: %d" % changed, file=out)
    return code


def running_guests(memguard, host):
    """(guest, PIDs, main PID or None) for each running guest.

    The main PID is the CT init or the kvm process.
    """
    result = []
    for vmid in host.ct_ids():
        pids = host.ct_pids(vmid)
        if pids is not None:
            init = next((pid for pid in pids if host.is_namespace_init(pid)), None)
            result.append((memguard.Guest("ct", vmid), pids, init))
    for vmid in host.vm_ids():
        pid = host.vm_pid(vmid)
        if pid is not None:
            result.append((memguard.Guest("vm", vmid), [pid], pid))
    return result


def cmd_apply(memguard, host, check, out):
    priorities = load_priorities(memguard, host)
    changed = 0
    for guest, pids, _main in running_guests(memguard, host):
        target = priorities.value_of(guest)
        written = kept = 0
        for pid in pids:
            current = host.oom_score_adj(pid)
            if current is None:
                continue
            if settled(current, target):
                if current != target:
                    kept += 1
                continue
            if check or host.write_oom_score_adj(pid, target, floor=target < 0):
                written += 1
        print("%s %d: %s %d of %d processes, %d kept a higher value" % (
            guest, target, "would set" if check else "set", written, len(pids), kept), file=out)
        changed += written
    print("changed: %d" % changed, file=out)
    return 0


def ct_config_problems(host, priorities, guest):
    problems = []
    value = priorities.value_of(guest)
    if host.read_or_none(include_path(guest.vmid)) != include_file_text(
            guest.vmid, priorities.tier_of(guest), value):
        expected = ("lxc.hook.start-host" if value > 0
                    else "lxc.proc.oom_score_adj" if value < 0 else "no setting")
        problems.append("%s missing or outdated (expected %s), run `pve-oom-priority sync`"
                        % (include_path(guest.vmid), expected))
    text = host.read_or_none("%s/%d.conf" % (PVE_LXC_DIR, guest.vmid)) or ""
    if not has_include(text.splitlines(), guest.vmid):
        problems.append("no `%s` in the main section, run `pve-oom-priority include %d`"
                        % (include_line(guest.vmid).strip(), guest.vmid))
    return problems


def vm_config_problems(host, guest):
    text = host.read_or_none("%s/%d.conf" % (PVE_QEMU_DIR, guest.vmid)) or ""
    hookscript = config_value(text, "hookscript")
    if hookscript != HOOKSCRIPT:
        return ["hookscript is %s, expected %s" % (hookscript or "unset", HOOKSCRIPT)]
    return []


def process_problems(host, target, pids, main):
    problems = []
    if main is None:
        problems.append("CT init not found")
    for pid in pids:
        current = host.oom_score_adj(pid)
        if current is None:
            continue
        if pid == main and not settled(current, target):
            problems.append("main PID %d has %d" % (pid, current))
        elif current < target:
            problems.append("PID %d has %d, run `pve-oom-priority apply`" % (pid, current))
    return problems


def lxc_hook_problems(host):
    """The start-host wrapper must be executable if an include file names it."""
    directory = host.path(INCLUDE_DIR)
    try:
        names = sorted(os.listdir(directory))
    except FileNotFoundError:
        return []
    users = [name for name in names if CONF_NAME.match(name)
             and LXC_HOOK in (host.read_or_none("%s/%s" % (INCLUDE_DIR, name)) or "")]
    if users and not os.access(host.path(LXC_HOOK), os.X_OK):
        return ["%s missing or not executable, used by %s/{%s}"
                % (LXC_HOOK, INCLUDE_DIR, ",".join(users))]
    return []


def cmd_check(memguard, host, out):
    priorities = load_priorities(memguard, host)
    running = {guest: (pids, main) for guest, pids, main in running_guests(memguard, host)}
    all_problems = 0
    print("%-8s %6s %-8s %-6s %s" % ("GUEST", "VALUE", "STATE", "CONFIG", "PROCESSES"), file=out)
    host_problems = []
    if host.vm_ids() and not os.access(host.path(HOOKSCRIPT_FILE), os.X_OK):
        host_problems.append("%s missing or not executable" % HOOKSCRIPT_FILE)
    host_problems += lxc_hook_problems(host)
    for guest in host_guests(memguard, host):
        target = priorities.value_of(guest)
        if guest.kind == "ct":
            config = ct_config_problems(host, priorities, guest)
        else:
            config = vm_config_problems(host, guest)
        if guest in running:
            pids, main = running[guest]
            processes = process_problems(host, target, pids, main)
            summary = "ok (%d)" % len(pids) if not processes else "BAD"
        else:
            processes = []
            summary = "-"
        print("%-8s %6d %-8s %-6s %s" % (
            guest, target, "running" if guest in running else "stopped",
            "BAD" if config else "ok", summary), file=out)
        for problem in config + processes:
            print("  %s" % problem, file=out)
        all_problems += len(config) + len(processes)
    for problem in host_problems:
        print(problem, file=out)
    all_problems += len(host_problems)
    print("problems: %d" % all_problems, file=out)
    return 1 if all_problems else 0


def cmd_hook(memguard_path, host, vmid, phase, err):
    """Always 0: a failing hookscript must never break a VM start."""
    if phase != "post-start":
        return 0
    try:
        memguard = load_memguard(memguard_path)
        vmid = int(vmid)
        target = load_priorities(memguard, host).value_of(memguard.Guest("vm", vmid))
        pid = host.vm_pid(vmid)
        if pid is None:
            print("pve-oom-priority: VM %d: no kvm process found" % vmid, file=err)
        elif host.oom_score_adj(pid) != target:
            host.write_oom_score_adj(pid, target, floor=target < 0)
    except Exception as exc:  # noqa: BLE001 - see the docstring
        print("pve-oom-priority: VM %s %s: %s" % (vmid, phase, exc), file=err)
    return 0


def cmd_lxc_start_host(memguard_path, host, args, environ, err):
    """lxc.hook.start-host entry; always 0, so it never blocks a CT start.

    lxc runs it in the host namespaces after the CT init is created and
    before it is woken, with the init's host PID in LXC_PID and the CT name
    (the vmid) in LXC_NAME; hook version 0 also passes NAME lxc start-host.
    """
    try:
        vmid = int(environ.get("LXC_NAME") or (args[0] if args else ""))
        pid = int(environ.get("LXC_PID", ""))
        memguard = load_memguard(memguard_path)
        target = load_priorities(memguard, host).value_of(memguard.Guest("ct", vmid))
        if not host.write_oom_score_adj(pid, target, floor=target < 0):
            print("pve-oom-priority: CT %d: init PID %d is gone" % (vmid, pid), file=err)
    except Exception as exc:  # noqa: BLE001 - see the docstring
        print("pve-oom-priority: lxc-start-host %r: %s" % (args, exc), file=err)
    return 0


def parse_args(argv):
    parser = argparse.ArgumentParser(
        prog="pve-oom-priority", description="Per-guest oom_score_adj from pve-memguard's kill order.")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("values", help="print guest, tier and value")
    for name, text in (("sync", "write the include file of every CT"),
                       ("apply", "set the value on the processes of running guests")):
        commands.add_parser(name, help=text).add_argument("--check", action="store_true")
    include = commands.add_parser("include", help="add the lxc.include line to CT configs")
    include.add_argument("--check", action="store_true")
    include.add_argument("vmids", nargs="+", type=int, metavar="VMID")
    commands.add_parser("check", help="verify; exit 1 on a problem")
    hook = commands.add_parser("hook", help="qm hookscript entry")
    hook.add_argument("vmid")
    hook.add_argument("phase")
    commands.add_parser("lxc-start-host", help="lxc.hook.start-host entry")
    return parser.parse_args(argv)


def main(argv=None, host=None, memguard_path=MEMGUARD_PATH, out=None, err=None, environ=None):
    out = sys.stdout if out is None else out
    err = sys.stderr if err is None else err
    argv = list(sys.argv[1:] if argv is None else argv)
    host = Host() if host is None else host
    # The hook entries never fail, not on their arguments either.
    if argv[:1] == ["hook"]:
        if len(argv) != 3:
            print("pve-oom-priority: hook needs VMID PHASE, got %r" % argv[1:], file=err)
            return 0
        return cmd_hook(memguard_path, host, argv[1], argv[2], err)
    if argv[:1] == ["lxc-start-host"]:
        return cmd_lxc_start_host(memguard_path, host, argv[1:],
                                  os.environ if environ is None else environ, err)
    args = parse_args(argv)
    try:
        memguard = load_memguard(memguard_path)
        if args.command == "values":
            return cmd_values(memguard, host, out)
        if args.command == "sync":
            return cmd_sync(memguard, host, args.check, out)
        if args.command == "include":
            return cmd_include(host, args.vmids, args.check, out, err)
        if args.command == "apply":
            return cmd_apply(memguard, host, args.check, out)
        return cmd_check(memguard, host, out)
    except (OSError, ValueError) as exc:
        print("pve-oom-priority: %s" % exc, file=err)
        return 1


if __name__ == "__main__":
    sys.exit(main())
