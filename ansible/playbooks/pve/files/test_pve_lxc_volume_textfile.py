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


if __name__ == "__main__":
    unittest.main()
