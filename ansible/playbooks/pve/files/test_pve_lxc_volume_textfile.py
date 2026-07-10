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
