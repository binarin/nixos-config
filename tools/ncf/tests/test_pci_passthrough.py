"""Tests for PCI passthrough helpers."""

import pytest

from ncf.commands.pci_passthrough import build_hostpci_spec, parse_lspci_output


class TestParseLspciOutput:
    """Tests for parsing lspci output to find PCI addresses."""

    SAMPLE_LSPCI = (
        "00:00.0 Host bridge: Advanced Micro Devices, Inc. [AMD] Starship/Matisse Root Complex\n"
        "0e:00.0 Non-Volatile memory controller: Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983\n"
        "0f:00.0 VGA compatible controller: NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)\n"
        "0f:00.1 Audio device: NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)\n"
    )

    def test_finds_unique_device(self):
        addr = parse_lspci_output(
            self.SAMPLE_LSPCI,
            "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983",
            all_functions=False,
        )
        assert addr == "0e:00.0"

    def test_all_functions_strips_function(self):
        addr = parse_lspci_output(
            self.SAMPLE_LSPCI,
            "Samsung Electronics Co Ltd NVMe SSD Controller SM981/PM981/PM983",
            all_functions=True,
        )
        assert addr == "0e:00"

    def test_fails_on_no_match(self):
        with pytest.raises(RuntimeError, match="No PCI device found"):
            parse_lspci_output(
                self.SAMPLE_LSPCI, "Nonexistent Device", all_functions=False
            )

    def test_fails_on_multiple_matches(self):
        with pytest.raises(RuntimeError, match="Multiple PCI devices found"):
            parse_lspci_output(
                self.SAMPLE_LSPCI, "NVIDIA Corporation GA102", all_functions=False
            )


class TestBuildHostpciSpec:
    """Tests for building Proxmox hostpci spec strings."""

    def test_basic_id_mode(self):
        spec = build_hostpci_spec(
            address="0e:00.0",
            mapping=None,
            pcie=True,
            primary_gpu=False,
            rom_bar=True,
            romfile=None,
        )
        assert spec == "0e:00.0,pcie=1,x-vga=0,rombar=1"

    def test_mapping_mode(self):
        spec = build_hostpci_spec(
            address=None,
            mapping="rtx-3090",
            pcie=True,
            primary_gpu=False,
            rom_bar=True,
            romfile=None,
        )
        assert spec == "mapping=rtx-3090,pcie=1,x-vga=0,rombar=1"

    def test_primary_gpu_with_rom(self):
        spec = build_hostpci_spec(
            address="0f:00.0",
            mapping=None,
            pcie=True,
            primary_gpu=True,
            rom_bar=True,
            romfile="llm-runner-pci-gpu.rom",
        )
        assert spec == "0f:00.0,pcie=1,x-vga=1,rombar=1,romfile=llm-runner-pci-gpu.rom"

    def test_no_pcie_no_rombar(self):
        spec = build_hostpci_spec(
            address="0e:00.0",
            mapping=None,
            pcie=False,
            primary_gpu=False,
            rom_bar=False,
            romfile=None,
        )
        assert spec == "0e:00.0,pcie=0,x-vga=0,rombar=0"


from ncf.commands.update_proxmox_vm import compute_diff


class TestComputeDiff:
    """Tests for computing config diffs."""

    def test_no_changes(self):
        current = {"memory": 2048, "cores": 2}
        desired = {"memory": "2048", "cores": "2"}
        assert compute_diff(current, desired) == []

    def test_changed_value(self):
        current = {"memory": 2048, "cores": 2}
        desired = {"memory": "65536", "cores": "2"}
        changes = compute_diff(current, desired)
        assert changes == [("memory", "2048", "65536")]

    def test_new_key(self):
        current = {"memory": 2048}
        desired = {"memory": "2048", "hostpci0": "0f:00.0,pcie=1"}
        changes = compute_diff(current, desired)
        assert changes == [("hostpci0", "(none)", "0f:00.0,pcie=1")]

    def test_stale_hostpci_removed(self):
        current = {
            "memory": 2048,
            "hostpci0": "0f:00.0,pcie=1",
            "hostpci1": "0e:00.0,pcie=1",
        }
        desired = {"memory": "2048", "hostpci0": "0f:00.0,pcie=1"}
        changes = compute_diff(current, desired)
        assert changes == [("hostpci1", "0e:00.0,pcie=1", "(removed)")]

    def test_mixed_changes(self):
        current = {"memory": 2048, "hostpci0": "old,pcie=1", "hostpci5": "stale"}
        desired = {"memory": "65536", "hostpci0": "new,pcie=1"}
        changes = compute_diff(current, desired)
        assert ("memory", "2048", "65536") in changes
        assert ("hostpci0", "old,pcie=1", "new,pcie=1") in changes
        assert ("hostpci5", "stale", "(removed)") in changes


from ncf.commands.update_proxmox_vm import compute_boot_order


class TestComputeBootOrder:
    """Tests for computing boot order from NixOS config."""

    def test_pci_device_bootable(self):
        """A PCI passthrough device marked bootable produces correct boot order."""
        vm_config = {
            "pci-passthrough": {
                "gpu-sound": {"bootable": False},
                "nvme": {"bootable": True},
            },
            "disks": [],
        }
        # PCI devices are sorted alphabetically: gpu-sound=hostpci0, nvme=hostpci1
        assert compute_boot_order(vm_config) == "order=hostpci1"

    def test_disk_bootable(self):
        """A disk with bootOrder set produces correct boot order."""
        vm_config = {
            "pci-passthrough": {},
            "disks": [
                {"bus": "scsi", "index": 0, "bootOrder": 1},
            ],
        }
        assert compute_boot_order(vm_config) == "order=scsi0"

    def test_no_bootable_device_returns_none(self):
        """When no device is marked bootable, returns None."""
        vm_config = {
            "pci-passthrough": {"nvme": {"bootable": False}},
            "disks": [{"bus": "scsi", "index": 0, "bootOrder": None}],
        }
        assert compute_boot_order(vm_config) is None

    def test_multiple_bootable_devices_raises(self):
        """More than one bootable device raises RuntimeError."""
        vm_config = {
            "pci-passthrough": {"nvme": {"bootable": True}},
            "disks": [{"bus": "scsi", "index": 0, "bootOrder": 1}],
        }
        with pytest.raises(RuntimeError, match="exactly 1 bootable device"):
            compute_boot_order(vm_config)

    def test_multiple_pci_bootable_raises(self):
        """Two PCI devices marked bootable raises RuntimeError."""
        vm_config = {
            "pci-passthrough": {
                "nvme1": {"bootable": True},
                "nvme2": {"bootable": True},
            },
            "disks": [],
        }
        with pytest.raises(RuntimeError, match="exactly 1 bootable device"):
            compute_boot_order(vm_config)


from ncf.commands.update_proxmox_vm import add_cleanup_entries


class TestAddCleanupEntries:
    """Tests for adding IDE cleanup entries to desired config."""

    def test_adds_ide_deletions_when_present(self):
        """When ide0 and ide2 exist in current config, adds removal entries."""
        current = {
            "ide0": "local-zfs:vm-125-cloudinit,media=cdrom",
            "ide2": "local:iso/nixos-installer.iso,media=cdrom,size=4563216K",
            "memory": "2048",
        }
        desired = {"memory": "2048"}
        add_cleanup_entries(current, desired)
        assert desired["ide0"] == "(removed)"
        assert desired["ide2"] == "(removed)"

    def test_skips_missing_ide_devices(self):
        """When ide0/ide2 are absent, does not add removal entries."""
        current = {"memory": "2048"}
        desired = {"memory": "2048"}
        add_cleanup_entries(current, desired)
        assert "ide0" not in desired
        assert "ide2" not in desired

    def test_partial_cleanup(self):
        """When only ide2 exists, only ide2 gets removal entry."""
        current = {
            "ide2": "local:iso/nixos-installer.iso,media=cdrom",
            "memory": "2048",
        }
        desired = {"memory": "2048"}
        add_cleanup_entries(current, desired)
        assert "ide0" not in desired
        assert desired["ide2"] == "(removed)"
