import io
import json
import os
import tempfile
import unittest

import pve_oom_priority as p

MEMGUARD_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "pve_memguard.py")
m = p.load_memguard(MEMGUARD_PATH)

CT = lambda vmid: m.Guest("ct", vmid)  # noqa: E731
VM = lambda vmid: m.Guest("vm", vmid)  # noqa: E731

CT_CONF = """\
arch: amd64
hostname: web
memory: 2048
rootfs: local-zfs:subvol-123-disk-0,size=8G

[before-upgrade]
arch: amd64
hostname: web
snaptime: 1700000000
"""


class FakeRoot:
    """A temp dir laid out like the PVE host's /etc, /proc and cgroup tree."""

    def __init__(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = self.tmp.name
        self.host = p.Host(self.root)

    def cleanup(self):
        self.tmp.cleanup()

    def write(self, path, text):
        full = self.host.path(path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w", encoding="utf-8") as fh:
            fh.write(text)

    def read(self, path):
        return self.host.read(path)

    def memguard_config(self, overrides):
        self.write(p.MEMGUARD_CONFIG, json.dumps(overrides))

    def ct(self, vmid, conf=CT_CONF):
        self.write("%s/%d.conf" % (p.PVE_LXC_DIR, vmid), conf)

    def vm(self, vmid, hookscript=None):
        conf = "memory: 4096\n"
        if hookscript:
            conf += "hookscript: %s\n" % hookscript
        self.write("%s/%d.conf" % (p.PVE_QEMU_DIR, vmid), conf)

    def process(self, pid, value, nspid=None, cgroup="0::/"):
        nspid = [pid] if nspid is None else nspid
        self.write("%s/%d/oom_score_adj" % (p.PROC, pid), "%d\n" % value)
        self.write("%s/%d/status" % (p.PROC, pid),
                   "Name:\tx\nNSpid:\t%s\n" % "\t".join(map(str, nspid)))
        self.write("%s/%d/cgroup" % (p.PROC, pid), cgroup + "\n")

    def running_ct(self, vmid, init_pid, init_value, others=()):
        """CT init in ns/init.scope, (pid, value) others in ns/system.slice."""
        self.process(init_pid, init_value, nspid=[init_pid, 1])
        cg = "%s/lxc/%d" % (p.CGROUP_ROOT, vmid)
        self.write(cg + "/cgroup.procs", "")
        self.write(cg + "/ns/init.scope/cgroup.procs", "%d\n" % init_pid)
        for pid, value in others:
            self.process(pid, value, nspid=[pid, pid - init_pid + 1])
        self.write(cg + "/ns/system.slice/cgroup.procs", "".join("%d\n" % pid for pid, _ in others))

    def running_vm(self, vmid, pid, value):
        self.process(pid, value, cgroup="0::/qemu.slice/%d.scope" % vmid)
        self.write("%s/%d.pid" % (p.QEMU_PID_DIR, vmid), "%d\n" % pid)
        self.write("%s/qemu.slice/%d.scope/cgroup.procs" % (p.CGROUP_ROOT, vmid), "%d\n" % pid)

    def value(self, pid):
        return self.host.oom_score_adj(pid)

    def run(self, *argv):
        out, err = io.StringIO(), io.StringIO()
        code = p.main(list(argv), root=self.root, memguard_path=MEMGUARD_PATH, out=out, err=err)
        return code, out.getvalue(), err.getvalue()


class FakeRootTestCase(unittest.TestCase):
    def setUp(self):
        self.fake = FakeRoot()
        self.addCleanup(self.fake.cleanup)


class TestTierValues(unittest.TestCase):
    def test_default_order(self):
        priorities = p.Priorities(m.Config({}))
        values = {str(guest): priorities.value_of(guest) for tier in priorities.config.kill_order
                  for guest in tier}
        self.assertEqual(values, {
            "ct:123": 1000, "ct:111": 667, "ct:116": 333,
            "ct:103": 0, "ct:107": 0, "ct:121": 0, "ct:127": 0, "ct:129": 0, "vm:104": 0,
            "ct:124": -150, "vm:109": -300, "ct:112": -450, "ct:115": -600, "ct:101": -750,
        })

    def test_unlisted_guest_gets_default(self):
        priorities = p.Priorities(m.Config({}))
        self.assertEqual(priorities.value_of(CT(999)), 0)
        self.assertEqual(priorities.value_of(VM(998)), 0)

    def test_default_tier_first(self):
        self.assertEqual(p.tier_values(3, 0), [0, -375, -750])

    def test_default_tier_last(self):
        self.assertEqual(p.tier_values(3, 2), [1000, 500, 0])

    def test_only_default_tier(self):
        self.assertEqual(p.tier_values(1, 0), [0])

    def test_range(self):
        for count in range(1, 12):
            for default in range(count):
                values = p.tier_values(count, default)
                self.assertEqual(values[default], 0)
                self.assertEqual(values, sorted(values, reverse=True))
                self.assertLessEqual(max(values), 1000)
                self.assertGreaterEqual(min(values), -750)


class TestConfigFile(FakeRootTestCase):
    def test_missing_config_uses_daemon_defaults(self):
        priorities = p.load_priorities(m, self.fake.host)
        self.assertEqual(priorities.value_of(CT(123)), 1000)

    def test_kill_order_override(self):
        self.fake.memguard_config({"dry_run": False, "kill_order": [
            ["ct:101"], ["default"], ["ct:123"]]})
        priorities = p.load_priorities(m, self.fake.host)
        self.assertEqual(priorities.value_of(CT(101)), 1000)
        self.assertEqual(priorities.value_of(CT(111)), 0)
        self.assertEqual(priorities.value_of(CT(123)), -750)

    def test_bad_config_fails(self):
        self.fake.memguard_config({"kill_order": [["ct:101"]]})
        self.fake.ct(101)
        code, _out, err = self.fake.run("sync")
        self.assertEqual(code, 1)
        self.assertIn("default", err)


class TestValues(FakeRootTestCase):
    def test_lists_known_and_host_guests(self):
        self.fake.ct(555)
        code, out, _err = self.fake.run("values")
        self.assertEqual(code, 0)
        rows = {line.split()[0]: line.split()[1:] for line in out.splitlines()[1:]}
        self.assertEqual(rows["ct:123"], ["0", "1000"])
        self.assertEqual(rows["ct:101"], ["8", "-750"])
        self.assertEqual(rows["ct:555"], ["3", "0"])
        self.assertEqual(rows["unlisted"], ["3", "0"])


class TestSync(FakeRootTestCase):
    def setUp(self):
        super().setUp()
        for vmid in (101, 123, 555):
            self.fake.ct(vmid)

    def test_writes_every_ct_then_nothing(self):
        code, out, _err = self.fake.run("sync")
        self.assertEqual(code, 0)
        self.assertEqual(out.splitlines()[-1], "changed: 3")
        self.assertIn("lxc.proc.oom_score_adj = -750\n", self.fake.read(p.include_path(101)))
        self.assertIn("lxc.proc.oom_score_adj = 1000\n", self.fake.read(p.include_path(123)))
        self.assertIn("lxc.proc.oom_score_adj = 0\n", self.fake.read(p.include_path(555)))
        self.assertEqual(self.fake.run("sync")[1], "changed: 0\n")

    def test_check_mode_writes_nothing(self):
        code, out, _err = self.fake.run("sync", "--check")
        self.assertEqual(code, 0)
        self.assertIn("would write /etc/lxc/oom-priority/101.conf (oom_score_adj -750)", out)
        self.assertEqual(out.splitlines()[-1], "changed: 3")
        self.assertFalse(self.fake.host.exists(p.INCLUDE_DIR))

    def test_rewrites_when_order_changes(self):
        self.fake.run("sync")
        self.fake.memguard_config({"kill_order": [["ct:101"], ["default"]]})
        code, out, _err = self.fake.run("sync")
        self.assertEqual(out.splitlines()[-1], "changed: 3")
        self.assertIn("= 1000\n", self.fake.read(p.include_path(101)))

    def test_keeps_files_of_removed_cts(self):
        self.fake.run("sync")
        os.unlink(self.fake.host.path("%s/555.conf" % p.PVE_LXC_DIR))
        self.assertEqual(self.fake.run("sync")[1], "changed: 0\n")
        self.assertTrue(self.fake.host.exists(p.include_path(555)))


class TestInclude(FakeRootTestCase):
    def setUp(self):
        super().setUp()
        self.fake.ct(123)
        self.fake.run("sync")
        self.conf = "%s/123.conf" % p.PVE_LXC_DIR

    def test_inserts_before_first_section(self):
        code, out, _err = self.fake.run("include", "123")
        self.assertEqual(code, 0)
        self.assertEqual(self.fake.read(self.conf), CT_CONF.replace(
            "size=8G\n\n[", "size=8G\nlxc.include: /etc/lxc/oom-priority/123.conf\n\n["))
        self.assertIn("+lxc.include: /etc/lxc/oom-priority/123.conf", out)
        self.assertEqual(out.splitlines()[-1], "changed: 1")
        self.assertTrue(self.fake.host.exists(p.LXC_CONFIG_LOCK % 123))

    def test_idempotent(self):
        self.fake.run("include", "123")
        before = self.fake.read(self.conf)
        code, out, _err = self.fake.run("include", "123")
        self.assertEqual(code, 0)
        self.assertEqual(self.fake.read(self.conf), before)
        self.assertEqual(out.splitlines()[-1], "changed: 0")

    def test_include_in_snapshot_only_still_added_to_main(self):
        self.fake.ct(123, CT_CONF + "lxc.include: /etc/lxc/oom-priority/123.conf\n")
        self.fake.run("include", "123")
        lines = self.fake.read(self.conf).splitlines()
        self.assertTrue(p.has_include(lines, 123))
        self.assertEqual(lines.count("lxc.include: /etc/lxc/oom-priority/123.conf"), 2)

    def test_no_sections_no_trailing_newline(self):
        self.assertEqual(p.with_include("arch: amd64", 1),
                         "arch: amd64\nlxc.include: /etc/lxc/oom-priority/1.conf\n")

    def test_pending_section(self):
        text = "arch: amd64\n[pve:pending]\nmemory: 1024\n"
        self.assertEqual(p.with_include(text, 1), (
            "arch: amd64\nlxc.include: /etc/lxc/oom-priority/1.conf\n[pve:pending]\nmemory: 1024\n"))

    def test_check_mode_changes_nothing(self):
        code, out, _err = self.fake.run("include", "--check", "123")
        self.assertEqual(code, 0)
        self.assertIn("+lxc.include:", out)
        self.assertEqual(self.fake.read(self.conf), CT_CONF)
        self.assertEqual(os.listdir(self.fake.host.path(p.PVE_LXC_DIR)), ["123.conf"])

    def test_refuses_without_include_file(self):
        self.fake.ct(124)
        code, _out, err = self.fake.run("include", "123", "124")
        self.assertEqual(code, 1)
        self.assertIn("refused: CT 124", err)
        # Nothing is changed, not even the CT that could have been.
        self.assertEqual(self.fake.read(self.conf), CT_CONF)

    def test_refuses_unknown_ct(self):
        code, _out, err = self.fake.run("include", "999")
        self.assertEqual(code, 1)
        self.assertIn("refused: CT 999: no /etc/pve/lxc/999.conf", err)


class TestSettled(unittest.TestCase):
    def test_rule(self):
        self.assertFalse(p.settled(0, 1000))
        self.assertFalse(p.settled(0, -750))
        self.assertFalse(p.settled(300, 1000))
        self.assertFalse(p.settled(-900, -150))
        self.assertTrue(p.settled(1000, 1000))
        self.assertTrue(p.settled(300, -150))
        self.assertTrue(p.settled(300, 0))
        self.assertTrue(p.settled(0, 0))


class TestApply(FakeRootTestCase):
    def setUp(self):
        super().setUp()
        self.fake.ct(123)
        self.fake.ct(101)
        self.fake.ct(555)  # stopped
        self.fake.vm(109)
        self.fake.vm(104)  # stopped
        self.fake.running_ct(123, 1000, 0, [(1001, 0), (1002, 300), (1003, 1000)])
        self.fake.running_ct(101, 2000, 0, [(2001, 300), (2002, -900)])
        self.fake.running_vm(109, 3000, 0)

    def test_rule_per_process(self):
        code, out, _err = self.fake.run("apply")
        self.assertEqual(code, 0)
        self.assertEqual([self.fake.value(pid) for pid in (1000, 1001, 1002, 1003)],
                         [1000, 1000, 1000, 1000])
        self.assertEqual([self.fake.value(pid) for pid in (2000, 2001, 2002)], [-750, 300, -750])
        self.assertEqual(self.fake.value(3000), -300)
        self.assertIn("ct:101 -750: set 2 of 3 processes, 1 kept a higher value", out)
        self.assertEqual(out.splitlines()[-1], "changed: 6")
        self.assertEqual(self.fake.run("apply")[1].splitlines()[-1], "changed: 0")

    def test_check_mode_writes_nothing(self):
        code, out, _err = self.fake.run("apply", "--check")
        self.assertEqual(code, 0)
        self.assertIn("ct:123 1000: would set 3 of 4 processes", out)
        self.assertEqual(out.splitlines()[-1], "changed: 6")
        self.assertEqual(self.fake.value(1000), 0)
        self.assertEqual(self.fake.value(3000), 0)

    def test_vanished_pid(self):
        cg = "%s/lxc/123/ns/system.slice/cgroup.procs" % p.CGROUP_ROOT
        self.fake.write(cg, "1001\n4242\n")
        code, out, _err = self.fake.run("apply")
        self.assertEqual(code, 0)
        self.assertEqual(self.fake.value(1001), 1000)

    def test_vanishing_on_write(self):
        self.fake.host.set_oom_score_adj = lambda pid, value: False
        out = io.StringIO()
        self.assertEqual(p.cmd_apply(m, self.fake.host, False, out), 0)
        self.assertEqual(out.getvalue().splitlines()[-1], "changed: 0")

    def test_stale_vm_pid_file(self):
        self.fake.process(3000, 0, cgroup="0::/system.slice/other.service")
        self.fake.run("apply")
        self.assertEqual(self.fake.value(3000), 0)


class TestSetOomScoreAdj(FakeRootTestCase):
    def test_gone(self):
        self.assertFalse(self.fake.host.set_oom_score_adj(4242, 5))

    def test_esrch_is_gone(self):
        def esrch(*_args, **_kwargs):
            raise ProcessLookupError(p.errno.ESRCH, "No such process")
        p.open = esrch  # shadows the builtin inside the module
        try:
            self.assertFalse(self.fake.host.set_oom_score_adj(1, 5))
        finally:
            del p.open


class TestCheck(FakeRootTestCase):
    def setUp(self):
        super().setUp()
        self.fake.ct(123)
        self.fake.ct(555)
        self.fake.vm(109, hookscript=p.HOOKSCRIPT)
        self.fake.write(p.HOOKSCRIPT_FILE, "#!/bin/sh\n")
        os.chmod(self.fake.host.path(p.HOOKSCRIPT_FILE), 0o755)
        self.fake.running_ct(123, 1000, 0, [(1001, 0), (1002, 300)])
        self.fake.running_vm(109, 3000, 0)
        self.fake.run("sync")
        self.fake.run("include", "123", "555")
        self.fake.run("apply")

    def test_all_good(self):
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 0, out)
        self.assertIn("ct:123     1000 running  ok     ok (3)", out)
        self.assertIn("ct:555        0 stopped  ok     -", out)
        self.assertEqual(out.splitlines()[-1], "problems: 0")

    def test_outdated_include_file(self):
        self.fake.write(p.include_path(555), "lxc.proc.oom_score_adj = 5\n")
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("/etc/lxc/oom-priority/555.conf missing or outdated", out)

    def test_missing_include_line(self):
        self.fake.ct(555)
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("run `pve-oom-priority include 555`", out)

    def test_process_below_target(self):
        self.fake.process(1001, 300, nspid=[1001, 2])
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("PID 1001 has 300", out)

    def test_init_not_at_target(self):
        self.fake.process(1000, 0, nspid=[1000, 1])
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("main PID 1000 has 0", out)

    def test_kvm_not_at_target(self):
        self.fake.process(3000, 0, cgroup="0::/qemu.slice/109.scope")
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("main PID 3000 has 0", out)

    def test_hookscript_missing(self):
        self.fake.vm(109, hookscript="local:snippets/other.pl")
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("hookscript is local:snippets/other.pl, expected " + p.HOOKSCRIPT, out)

    def test_snippet_missing(self):
        os.unlink(self.fake.host.path(p.HOOKSCRIPT_FILE))
        code, out, _err = self.fake.run("check")
        self.assertEqual(code, 1)
        self.assertIn("oom-priority-hook.sh missing or not executable", out)


class TestHook(FakeRootTestCase):
    def setUp(self):
        super().setUp()
        self.fake.vm(109, hookscript=p.HOOKSCRIPT)
        self.fake.running_vm(109, 3000, 0)

    def test_post_start_sets_value(self):
        self.assertEqual(self.fake.run("hook", "109", "post-start"), (0, "", ""))
        self.assertEqual(self.fake.value(3000), -300)

    def test_other_phases_do_nothing(self):
        for phase in ("pre-start", "pre-stop", "post-stop"):
            self.assertEqual(self.fake.run("hook", "109", phase)[0], 0)
        self.assertEqual(self.fake.value(3000), 0)

    def test_always_exits_zero(self):
        os.unlink(self.fake.host.path("%s/109.pid" % p.QEMU_PID_DIR))
        code, _out, err = self.fake.run("hook", "109", "post-start")
        self.assertEqual(code, 0)
        self.assertIn("no kvm process", err)
        self.fake.memguard_config({"kill_order": "broken"})
        code, _out, err = self.fake.run("hook", "109", "post-start")
        self.assertEqual(code, 0)
        self.assertIn("kill_order", err)
        self.assertEqual(self.fake.run("hook", "notanumber", "post-start")[0], 0)
        self.assertEqual(self.fake.run("hook")[0], 0)

    def test_missing_memguard_exits_zero(self):
        out, err = io.StringIO(), io.StringIO()
        code = p.main(["hook", "109", "post-start"], root=self.fake.root,
                      memguard_path=self.fake.host.path("/nonexistent"), out=out, err=err)
        self.assertEqual(code, 0)
        self.assertIn("pve-oom-priority: VM 109 post-start", err.getvalue())


if __name__ == "__main__":
    unittest.main()
