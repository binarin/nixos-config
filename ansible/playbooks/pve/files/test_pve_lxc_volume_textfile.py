import unittest
import os
import tempfile
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

    def test_empty_hostname_falls_back(self):
        text = "hostname:\nrootfs: local-zfs:subvol-107-disk-0,size=8G\n"
        recs = m.parse_ct_config("107", text)
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

    def test_duplicate_basename_warns_and_last_wins(self):
        import io
        import contextlib

        stderr = io.StringIO()
        with contextlib.redirect_stderr(stderr):
            mp = m.parse_zfs_list(
                "rpool/data/subvol-112-disk-0\t/mnt/first\n"
                "rpool/backup/subvol-112-disk-0\t/mnt/second\n"
            )
        captured = stderr.getvalue()
        self.assertIn("warning: duplicate volume basename", captured)
        self.assertIn("subvol-112-disk-0", captured)
        self.assertEqual(
            mp["subvol-112-disk-0"],
            ("rpool/backup/subvol-112-disk-0", "/mnt/second"),
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

    def test_help_line_present(self):
        recs = [m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/")]
        out = m.render_prom(recs, self._map())
        self.assertIn("# HELP pve_lxc_volume_info", out)

    def test_all_unmounted_sentinels_skipped(self):
        for sentinel in ["", "-", "none", "legacy"]:
            with self.subTest(sentinel=sentinel):
                recs = [m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/")]
                out = m.render_prom(
                    recs,
                    {"subvol-112-disk-0": ("rpool/data/subvol-112-disk-0", sentinel)},
                )
                self.assertNotIn("forgejo", out)

    def test_deduplicates_identical_labels(self):
        recs = [
            m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/"),
            m.VolumeRecord("112", "forgejo", "rootfs", "subvol-112-disk-0", "/"),
        ]
        out = m.render_prom(recs, self._map())
        self.assertEqual(out.count("pve_lxc_volume_info{"), 1)

    def test_escapes_backslash_quote_newline(self):
        guest = chr(92) + chr(34) + chr(10)  # backslash, double-quote, newline
        escaped_guest = m._escape(guest)
        recs = [m.VolumeRecord("112", guest, "rootfs", "subvol-112-disk-0", "/")]
        out = m.render_prom(recs, self._map())
        self.assertIn('guest="' + escaped_guest + '"', out)


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

    def test_written_file_is_world_readable(self):
        import stat

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "out.prom")
            m._atomic_write(path, "hello\n")
            self.assertEqual(stat.S_IMODE(os.stat(path).st_mode), 0o644)


if __name__ == "__main__":
    unittest.main()
