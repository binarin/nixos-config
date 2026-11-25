import json
import os
import stat
import tempfile
import unittest

import pve_memguard as m

GIB = m.GIB
MIB = 1 << 20

CT = lambda vmid: m.Guest("ct", vmid)  # noqa: E731
VM = lambda vmid: m.Guest("vm", vmid)  # noqa: E731

MEMINFO = """\
MemTotal:       65756048 kB
MemFree:          812000 kB
MemAvailable:    1048576 kB
Buffers:               0 kB
SwapTotal:       8388608 kB
SwapFree:         524288 kB
HugePages_Total:       0
"""

ARCSTATS = """\
13 1 0x01 147 39984 4205839427 1047218862224390
name                            type data
hits                            4    123456
size                            4    3221225472
c_min                           4    2147483648
c_max                           4    63000000000
"""

PSI = """\
some avg10=12.50 avg60=3.00 avg300=1.00 total=123456
full avg10=8.25 avg60=2.00 avg300=0.50 total=65432
"""

PSI_CALM = """\
some avg10=0.10 avg60=0.00 avg300=0.00 total=123456
full avg10=0.00 avg60=0.00 avg300=0.00 total=65432
"""


def meminfo(available, swap_free=0):
    return "MemAvailable: %d kB\nSwapFree: %d kB\n" % (available // 1024, swap_free // 1024)


def arcstats(size, c_min):
    return "name type data\nsize 4 %d\nc_min 4 %d\n" % (size, c_min)


class TestGuest(unittest.TestCase):
    def test_parse_and_format(self):
        self.assertEqual(m.Guest.parse("ct:123"), CT(123))
        self.assertEqual(m.Guest.parse("vm:109"), VM(109))
        self.assertEqual(str(VM(109)), "vm:109")
        self.assertEqual(CT(123).label, "CT 123")

    def test_parse_rejects_garbage(self):
        for ref in ["123", "lxc:123", "ct:", "ct:12a", "ct:١٢٣", "default"]:
            with self.subTest(ref=ref):
                with self.assertRaises(ValueError):
                    m.Guest.parse(ref)


class TestConfig(unittest.TestCase):
    def test_defaults(self):
        cfg = m.load_config({})
        self.assertTrue(cfg.dry_run)
        self.assertEqual(cfg.psi_trigger, "full 1000000 10000000")
        self.assertEqual(cfg.headroom_threshold_bytes, 3 * GIB)
        self.assertEqual(cfg.kill_cap, 3)
        self.assertEqual(cfg.kill_cap_window_seconds, 3600)
        self.assertEqual(cfg.restart_delay_seconds, 600)
        self.assertEqual(cfg.restart_retry_seconds, 300)
        self.assertEqual(cfg.default_tier, 3)

    def test_overrides(self):
        cfg = m.load_config({"dry_run": False, "settle_seconds": 30,
                             "restart_max_some_avg10": 10})
        self.assertFalse(cfg.dry_run)
        self.assertEqual(cfg.settle_seconds, 30)
        self.assertEqual(cfg.restart_max_some_avg10, 10.0)

    def test_rejects_bad_values(self):
        bad = [
            {"no_such_key": 1},
            {"dry_run": "false"},
            {"settle_seconds": 0},
            {"settle_seconds": True},
            {"kill_cap": 1.5},
            {"psi_trigger_kind": "half"},
            {"psi_trigger_window_us": 20_000_000},
            {"psi_trigger_stall_us": 2_000_000, "psi_trigger_window_us": 1_000_000},
            {"restart_max_some_avg10": 101},
            {"state_file": "relative/state.json"},
            {"kill_order": []},
            {"kill_order": [["ct:1"]]},                       # no default tier
            {"kill_order": [["ct:1", "default"], ["default"]]},
            {"kill_order": [["ct:1"], ["ct:1", "default"]]},  # duplicate guest
            {"kill_order": [[], ["default"]]},
            {"kill_order": [["ct:1", 2, "default"]]},
        ]
        for data in bad:
            with self.subTest(data=data):
                with self.assertRaises(ValueError):
                    m.load_config(data)

    def test_rejects_non_object(self):
        with self.assertRaises(ValueError):
            m.load_config([])

    def test_load_config_file(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "config.json")
            with open(path, "w") as fh:
                json.dump({"dry_run": False}, fh)
            self.assertFalse(m.load_config_file(path).dry_run)

    def test_default_order_matches_brief(self):
        # Brief, most important first: 101, 115, 112, VM 109, 124, default
        # (103, 107, 121, 127, 129, VM 104), 116, 111, 123.
        cfg = m.load_config({})
        flat = [g for tier in cfg.kill_order for g in tier]
        self.assertEqual(
            flat,
            [CT(123), CT(111), CT(116), CT(103), CT(107), CT(121), CT(127), CT(129),
             VM(104), CT(124), VM(109), CT(112), CT(115), CT(101)],
        )


class TestHeadroom(unittest.TestCase):
    def test_parse_meminfo_converts_kb(self):
        info = m.parse_meminfo(MEMINFO)
        self.assertEqual(info["MemAvailable"], 1 * GIB)
        self.assertEqual(info["SwapFree"], 512 * MIB)
        self.assertEqual(info["HugePages_Total"], 0)

    def test_parse_arcstats_skips_headers(self):
        arc = m.parse_arcstats(ARCSTATS)
        self.assertEqual(arc["size"], 3 * GIB)
        self.assertEqual(arc["c_min"], 2 * GIB)
        self.assertNotIn("name", arc)
        self.assertNotIn("13", arc)

    def test_parse_psi(self):
        psi = m.parse_psi(PSI)
        self.assertEqual(psi["some"]["avg10"], 12.5)
        self.assertEqual(psi["full"]["avg10"], 8.25)
        self.assertEqual(psi["full"]["total"], 65432)

    def test_sum_of_available_arc_spare_and_swap(self):
        headroom = m.headroom_bytes(m.parse_meminfo(MEMINFO), m.parse_arcstats(ARCSTATS))
        self.assertEqual(headroom, 1 * GIB + 1 * GIB + 512 * MIB)

    def test_arc_below_c_min_counts_zero(self):
        headroom = m.headroom_bytes(m.parse_meminfo(meminfo(GIB)),
                                    m.parse_arcstats(arcstats(GIB, 2 * GIB)))
        self.assertEqual(headroom, GIB)

    def test_no_zfs_no_swap(self):
        headroom = m.headroom_bytes(m.parse_meminfo("MemAvailable: 1024 kB\n"), {})
        self.assertEqual(headroom, 1024 * 1024)


class TestSelectVictim(unittest.TestCase):
    def setUp(self):
        self.cfg = m.load_config({})

    def test_least_important_running_guest_first(self):
        running = {CT(101): GIB, CT(123): GIB, CT(111): GIB, VM(109): GIB}
        self.assertEqual(m.select_victim(running, self.cfg), CT(123))

    def test_skips_guests_not_running(self):
        running = {CT(101): GIB, CT(116): GIB, CT(124): GIB}
        self.assertEqual(m.select_victim(running, self.cfg), CT(116))

    def test_unlisted_guest_is_in_default_tier(self):
        running = {CT(124): 8 * GIB, CT(150): GIB}
        self.assertEqual(m.select_victim(running, self.cfg), CT(150))
        running = {CT(116): GIB, CT(150): 8 * GIB}
        self.assertEqual(m.select_victim(running, self.cfg), CT(116))

    def test_biggest_first_within_a_tier(self):
        running = {CT(103): GIB, VM(104): 4 * GIB, CT(150): 2 * GIB}
        self.assertEqual(m.select_victim(running, self.cfg), VM(104))

    def test_goes_all_the_way_down(self):
        order = [CT(123), CT(111), CT(116), CT(103), CT(124), VM(109), CT(112), CT(115),
                 CT(101)]
        running = {g: GIB for g in order}
        killed = []
        while True:
            victim = m.select_victim(running, self.cfg, set(killed))
            if victim is None:
                break
            killed.append(victim)
        self.assertEqual(killed, order)

    def test_ct_and_vm_with_same_number_are_distinct(self):
        running = {VM(123): GIB}
        self.assertEqual(m.select_victim(running, self.cfg), VM(123))  # default tier

    def test_excluded_and_empty(self):
        self.assertIsNone(m.select_victim({}, self.cfg))
        self.assertIsNone(m.select_victim({CT(123): GIB}, self.cfg, {CT(123)}))


class TestLedger(unittest.TestCase):
    def setUp(self):
        self.cfg = m.load_config({})

    def test_kill_schedules_restart_after_delay(self):
        ledger = m.Ledger()
        self.assertFalse(ledger.record_kill(CT(123), 1000.0, self.cfg))
        self.assertEqual(ledger.pending[CT(123)].due, 1600.0)
        self.assertEqual(ledger.due_restarts(1599.0), [])
        self.assertEqual(ledger.due_restarts(1600.0), [CT(123)])
        self.assertEqual(ledger.next_due(), 1600.0)

    def test_third_kill_within_an_hour_holds_guest(self):
        ledger = m.Ledger()
        self.assertFalse(ledger.record_kill(CT(123), 0.0, self.cfg))
        self.assertFalse(ledger.record_kill(CT(123), 1200.0, self.cfg))
        self.assertTrue(ledger.record_kill(CT(123), 3599.0, self.cfg))
        self.assertIn(CT(123), ledger.held)
        self.assertNotIn(CT(123), ledger.pending)

    def test_kills_older_than_the_window_do_not_count(self):
        ledger = m.Ledger()
        ledger.record_kill(CT(123), 0.0, self.cfg)
        ledger.record_kill(CT(123), 1200.0, self.cfg)
        self.assertFalse(ledger.record_kill(CT(123), 3600.0, self.cfg))
        self.assertEqual(ledger.kills[CT(123)], [1200.0, 3600.0])
        self.assertNotIn(CT(123), ledger.held)

    def test_cap_is_per_guest(self):
        ledger = m.Ledger()
        ledger.record_kill(CT(123), 0.0, self.cfg)
        ledger.record_kill(CT(111), 1.0, self.cfg)
        self.assertFalse(ledger.record_kill(CT(123), 2.0, self.cfg))
        self.assertFalse(ledger.record_kill(CT(111), 3.0, self.cfg))

    def test_reschedule_counts_attempts(self):
        ledger = m.Ledger()
        ledger.record_kill(CT(123), 0.0, self.cfg)
        ledger.reschedule(CT(123), 900.0, attempted=False)
        ledger.reschedule(CT(123), 1200.0, attempted=True)
        self.assertEqual(ledger.pending[CT(123)], m.PendingRestart(due=1200.0, attempts=1))

    def test_prune(self):
        ledger = m.Ledger()
        ledger.record_kill(CT(123), 0.0, self.cfg)
        ledger.record_kill(CT(111), 3000.0, self.cfg)
        self.assertTrue(ledger.prune(3600.0, 3600))
        self.assertEqual(list(ledger.kills), [CT(111)])
        self.assertFalse(ledger.prune(3600.0, 3600))

    def test_json_round_trip(self):
        ledger = m.Ledger()
        ledger.record_kill(CT(123), 10.0, self.cfg)
        ledger.record_kill(VM(109), 20.0, self.cfg)
        ledger.reschedule(VM(109), 900.0, attempted=True)
        ledger.held.add(CT(111))
        copy = m.Ledger.from_json(ledger.to_json())
        self.assertEqual(copy.kills, ledger.kills)
        self.assertEqual(copy.pending, ledger.pending)
        self.assertEqual(copy.held, ledger.held)

    def test_cap_survives_a_restart_through_the_state_file(self):
        ledger = m.Ledger()
        ledger.record_kill(CT(123), 0.0, self.cfg)
        ledger.record_kill(CT(123), 100.0, self.cfg)
        restored = m.Ledger.from_json(ledger.to_json())
        self.assertTrue(restored.record_kill(CT(123), 200.0, self.cfg))

    def test_from_json_rejects_unknown_version(self):
        with self.assertRaises(ValueError):
            m.Ledger.from_json('{"version": 99}')


class FakeHost:
    def __init__(self):
        self.meminfo = meminfo(GIB)
        self.arcstats = arcstats(2 * GIB, 2 * GIB)
        self.psi = PSI
        self.running = {}
        self.killed = []
        self.started = []
        self.states = []
        self.metrics = []
        self.kill_error = None
        self.start_error = None

    def read_meminfo(self):
        return self.meminfo

    def read_arcstats(self):
        return self.arcstats

    def read_psi(self):
        return self.psi

    def running_guests(self):
        return dict(self.running)

    def kill(self, guest):
        if self.kill_error:
            raise self.kill_error
        self.killed.append(guest)

    def start(self, guest):
        if self.start_error:
            raise self.start_error
        self.started.append(guest)

    def write_state(self, text):
        self.states.append(text)

    def write_metrics(self, text):
        self.metrics.append(text)


class FakeLog:
    def __init__(self):
        self.infos = []
        self.events = []

    def info(self, message):
        self.infos.append(message)

    def event(self, message):
        self.events.append(message)


def guard(host=None, **config):
    cfg = m.load_config(config)
    return m.Memguard(cfg, host or FakeHost(), m.Ledger(), FakeLog())


class TestPressureEvent(unittest.TestCase):
    def test_headroom_ok_is_ignored(self):
        host = FakeHost()
        host.meminfo = meminfo(2 * GIB)
        host.arcstats = arcstats(3 * GIB, 2 * GIB)  # 1 GiB ARC spare -> 3 GiB total
        host.running = {CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        self.assertFalse(g.handle_pressure_event(100.0))
        self.assertEqual(host.killed, [])
        self.assertEqual(g.pressure_events["headroom_ok"], 1)
        self.assertEqual(g.log.events, [])
        self.assertEqual(g.last_trigger, 100.0)

    def test_free_swap_counts_as_headroom(self):
        host = FakeHost()
        host.meminfo = meminfo(GIB, swap_free=2 * GIB)
        host.running = {CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        self.assertFalse(g.handle_pressure_event(100.0))
        self.assertEqual(host.killed, [])

    def test_kill_least_important_and_schedule_restart(self):
        host = FakeHost()
        host.running = {CT(101): 4 * GIB, CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        self.assertTrue(g.handle_pressure_event(100.0))
        self.assertEqual(host.killed, [CT(123)])
        self.assertEqual(g.ledger.pending[CT(123)].due, 700.0)
        self.assertEqual(g.kills[CT(123)], 1)
        self.assertEqual(len(host.states), 1)
        self.assertIn("killed CT 123", g.log.events[-1])

    def test_dry_run_kills_nothing_and_schedules_nothing(self):
        host = FakeHost()
        host.running = {CT(101): 4 * GIB, CT(123): 20 * GIB}
        g = guard(host)
        self.assertTrue(g.handle_pressure_event(100.0))
        self.assertEqual(host.killed, [])
        self.assertEqual(g.ledger.pending, {})
        self.assertEqual(g.ledger.kills, {})
        self.assertEqual(host.states, [])
        self.assertEqual(g.would_kills[CT(123)], 1)
        self.assertEqual(g.pressure_events["would_kill"], 1)
        self.assertIn("dry-run", g.log.events[-1])
        self.assertIn("would kill CT 123", g.log.events[-1])

    def test_escalates_past_an_exited_victim(self):
        host = FakeHost()
        host.running = {CT(111): 4 * GIB, CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        g.handle_pressure_event(100.0)
        del host.running[CT(123)]
        g.handle_pressure_event(115.0)
        self.assertEqual(host.killed, [CT(123), CT(111)])

    def test_waits_for_an_exiting_victim_to_free_memory(self):
        host = FakeHost()
        host.running = {CT(111): 4 * GIB, CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        g.handle_pressure_event(100.0)
        # CT 123 still exits; its 20 GiB will come back without another kill.
        self.assertTrue(g.handle_pressure_event(115.0))
        self.assertEqual(host.killed, [CT(123)])
        self.assertEqual(g.pressure_events["awaiting_exit"], 1)

    def test_kills_next_if_exiting_victim_frees_too_little(self):
        host = FakeHost()
        host.running = {CT(111): 4 * GIB, CT(123): 512 * MIB}
        g = guard(host, dry_run=False)
        g.handle_pressure_event(100.0)
        g.handle_pressure_event(115.0)
        self.assertEqual(host.killed, [CT(123), CT(111)])

    def test_stuck_victim_is_killed_again_after_exit_grace(self):
        host = FakeHost()
        host.running = {CT(111): 4 * GIB, CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        g.handle_pressure_event(100.0)
        g.handle_pressure_event(100.0 + g.config.exit_grace_seconds)
        self.assertEqual(host.killed, [CT(123), CT(123)])

    def test_third_kill_holds_the_guest(self):
        host = FakeHost()
        host.running = {CT(123): 20 * GIB}
        g = guard(host, dry_run=False)
        for now in (0.0, 1000.0, 2000.0):
            g.handle_pressure_event(now)
        self.assertEqual(host.killed, [CT(123)] * 3)
        self.assertIn(CT(123), g.ledger.held)
        self.assertNotIn(CT(123), g.ledger.pending)
        self.assertIn("holding it stopped", g.log.events[-1])

    def test_no_victim_logs_and_does_not_panic(self):
        g = guard(dry_run=False)
        self.assertTrue(g.handle_pressure_event(100.0))
        self.assertEqual(g.pressure_events["no_victim"], 1)
        self.assertIn("no guest left to kill", g.log.events[-1])

    def test_failed_kill_is_not_recorded(self):
        host = FakeHost()
        host.running = {CT(123): 20 * GIB}
        host.kill_error = FileNotFoundError("gone")
        g = guard(host, dry_run=False)
        g.handle_pressure_event(100.0)
        self.assertEqual(g.ledger.kills, {})
        self.assertEqual(g.pressure_events["kill_failed"], 1)


class TestTick(unittest.TestCase):
    def killed_guard(self, **config):
        host = FakeHost()
        host.running = {CT(123): 20 * GIB}
        g = guard(host, dry_run=False, **config)
        g.handle_pressure_event(0.0)
        del host.running[CT(123)]
        host.psi = PSI_CALM
        host.meminfo = meminfo(10 * GIB)
        return g, host

    def test_no_start_before_the_delay(self):
        g, host = self.killed_guard()
        g.handle_tick(599.0)
        self.assertEqual(host.started, [])

    def test_start_after_the_delay_when_calm(self):
        g, host = self.killed_guard()
        g.handle_tick(600.0)
        self.assertEqual(host.started, [CT(123)])
        self.assertEqual(g.ledger.pending[CT(123)], m.PendingRestart(due=900.0, attempts=1))
        self.assertEqual(g.starts[CT(123)], 1)

    def test_retry_every_five_minutes_while_pressure_is_high(self):
        g, host = self.killed_guard()
        host.psi = PSI  # some avg10=12.5 > 5
        g.handle_tick(600.0)
        self.assertEqual(host.started, [])
        self.assertEqual(g.ledger.pending[CT(123)], m.PendingRestart(due=900.0, attempts=0))
        host.psi = PSI_CALM
        host.meminfo = meminfo(4 * GIB)  # below restart_min_headroom_bytes
        g.handle_tick(900.0)
        self.assertEqual(host.started, [])
        host.meminfo = meminfo(10 * GIB)
        g.handle_tick(1200.0)
        self.assertEqual(host.started, [CT(123)])

    def test_pending_cleared_once_running(self):
        g, host = self.killed_guard()
        g.handle_tick(600.0)
        host.running[CT(123)] = GIB
        g.handle_tick(900.0)
        self.assertEqual(g.ledger.pending, {})
        self.assertEqual(host.started, [CT(123)])

    def test_failed_start_is_retried(self):
        g, host = self.killed_guard()
        host.start_error = OSError("systemd-run failed")
        g.handle_tick(600.0)
        host.start_error = None
        g.handle_tick(900.0)
        self.assertEqual(host.started, [CT(123)])
        self.assertEqual(g.ledger.pending[CT(123)].attempts, 2)

    def test_dry_run_never_starts(self):
        g, host = self.killed_guard()
        g.config = m.load_config({"dry_run": True})
        g.handle_tick(600.0)
        self.assertEqual(host.started, [])
        self.assertIn(CT(123), g.ledger.pending)

    def test_held_guest_is_released_when_started_by_hand(self):
        g, host = self.killed_guard()
        g.ledger.pending.clear()
        g.ledger.held.add(CT(123))
        host.running[CT(123)] = GIB  # still exiting right after the kill
        g.handle_tick(10.0)
        self.assertIn(CT(123), g.ledger.held)
        g.handle_tick(600.0)
        self.assertNotIn(CT(123), g.ledger.held)
        self.assertEqual(host.started, [])

    def test_prunes_and_persists(self):
        g, host = self.killed_guard()
        g.ledger.pending.clear()
        g.handle_tick(3600.0)
        self.assertEqual(g.ledger.kills, {})
        self.assertEqual(m.Ledger.from_json(host.states[-1]).kills, {})

    def test_poll_timeout(self):
        g, _host = self.killed_guard()
        self.assertEqual(g.poll_timeout(0.0), 60)
        self.assertEqual(g.poll_timeout(590.0), 10.0)
        self.assertEqual(g.poll_timeout(700.0), 1.0)
        self.assertEqual(guard().poll_timeout(0.0), 60)


class TestMetrics(unittest.TestCase):
    def test_render(self):
        host = FakeHost()
        host.running = {CT(123): 20 * GIB, VM(109): GIB}
        g = guard(host, dry_run=False)
        g.handle_pressure_event(100.0)
        g.ledger.held.add(CT(111))
        out = g.render_metrics(130.0)
        self.assertIn("pve_memguard_dry_run 0\n", out)
        self.assertIn("pve_memguard_heartbeat_timestamp_seconds 130\n", out)
        self.assertIn("pve_memguard_last_trigger_timestamp_seconds 100\n", out)
        self.assertIn("pve_memguard_headroom_bytes %d\n" % GIB, out)
        self.assertIn('pve_memguard_pressure_events_total{decision="kill"} 1\n', out)
        self.assertIn('pve_memguard_pressure_events_total{decision="no_victim"} 0\n', out)
        self.assertIn('pve_memguard_kills_total{type="ct",vmid="123"} 1\n', out)
        self.assertIn('pve_memguard_restart_pending{type="ct",vmid="123"} 1\n', out)
        self.assertIn('pve_memguard_guest_held_stopped{type="ct",vmid="111"} 1\n', out)
        self.assertIn("# TYPE pve_memguard_kills_total counter", out)
        self.assertTrue(out.endswith("\n"))

    def test_render_dry_run(self):
        host = FakeHost()
        host.running = {CT(123): 20 * GIB}
        g = guard(host)
        g.handle_pressure_event(100.0)
        out = g.render_metrics(130.0)
        self.assertIn("pve_memguard_dry_run 1\n", out)
        self.assertIn('pve_memguard_would_kills_total{type="ct",vmid="123"} 1\n', out)
        self.assertNotIn("pve_memguard_kills_total{", out)
        self.assertNotIn("pve_memguard_restart_pending{", out)

    def test_metrics_write_error_is_logged_once(self):
        class BrokenHost(FakeHost):
            def write_metrics(self, text):
                raise FileNotFoundError("no textfile dir")

        g = guard(BrokenHost())
        g.write_metrics(1.0)
        g.write_metrics(2.0)
        self.assertEqual(len(g.log.events), 1)


class TestSysHost(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = self.tmp.name
        self.cg = os.path.join(self.root, "cgroup")
        self.cfg = m.load_config({
            "state_file": os.path.join(self.root, "state", "state.json"),
            "textfile_dir": self.root,
        })
        self.host = m.SysHost(self.cfg, proc_root=os.path.join(self.root, "proc"),
                              cgroup_root=self.cg)

    def tearDown(self):
        self.tmp.cleanup()

    def cgroup(self, *parts, memory=None):
        path = os.path.join(self.cg, *parts)
        os.makedirs(path)
        if memory is not None:
            with open(os.path.join(path, "memory.current"), "w") as fh:
                fh.write("%d\n" % memory)
        return path

    def test_running_guests(self):
        self.cgroup("lxc", "123", memory=5 * GIB)
        self.cgroup("lxc", "101")                  # unreadable memory.current
        self.cgroup("lxc", "notanid")
        self.cgroup("qemu.slice", "109.scope", memory=GIB)
        self.cgroup("qemu.slice", "something.slice")
        self.assertEqual(self.host.running_guests(), {CT(123): 5 * GIB, CT(101): 0, VM(109): GIB})

    def test_running_guests_without_cgroups(self):
        self.assertEqual(self.host.running_guests(), {})

    def test_kill_writes_cgroup_kill(self):
        ct = self.cgroup("lxc", "123")
        vm = self.cgroup("qemu.slice", "109.scope")
        self.host.kill(CT(123))
        self.host.kill(VM(109))
        for path in (ct, vm):
            with open(os.path.join(path, "cgroup.kill")) as fh:
                self.assertEqual(fh.read(), "1")

    def test_state_and_metrics_files(self):
        self.assertIsNone(self.host.read_state())
        self.host.write_state("{}\n")
        self.assertEqual(self.host.read_state(), "{}\n")
        self.assertEqual(stat.S_IMODE(os.stat(self.cfg.state_file).st_mode), 0o600)
        self.host.write_metrics("x 1\n")
        prom = os.path.join(self.root, m.METRICS_BASENAME)
        self.assertEqual(stat.S_IMODE(os.stat(prom).st_mode), 0o644)
        self.assertEqual(sorted(os.listdir(self.root)), sorted(["state", m.METRICS_BASENAME]))

    def test_arcstats_missing_without_zfs(self):
        self.assertEqual(self.host.read_arcstats(), "")

    def test_load_ledger_survives_a_corrupt_state_file(self):
        self.host.write_state("not json")
        log = FakeLog()
        ledger = m.load_ledger(self.host, log)
        self.assertEqual(ledger.kills, {})
        self.assertIn("state file unreadable", log.events[0])


class TestMain(unittest.TestCase):
    def test_check_config(self):
        with tempfile.TemporaryDirectory() as d:
            good = os.path.join(d, "good.json")
            bad = os.path.join(d, "bad.json")
            with open(good, "w") as fh:
                json.dump({"dry_run": True}, fh)
            with open(bad, "w") as fh:
                json.dump({"dry_run": "yes"}, fh)
            import contextlib
            import io
            with contextlib.redirect_stdout(io.StringIO()), \
                    contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(m.main(["--check-config", good]), 0)
                self.assertEqual(m.main(["--check-config", bad]), 1)
                self.assertEqual(m.main([]), 2)


if __name__ == "__main__":
    unittest.main()
