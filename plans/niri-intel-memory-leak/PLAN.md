# Investigate and fix shmem/unevictable memory leak causing OOM kills on furfur

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds. This document must be maintained in accordance with ../PLANS.md.

## Purpose / Big Picture

The Surface Pro (furfur) with 32 GB RAM and Intel Iris Xe integrated GPU has been experiencing OOM kills since late November 2025. During OOM events, up to 25 GB of system RAM is consumed by unevictable shared memory (shmem), leaving no room for normal applications. The compositor niri, the Wayland window manager, is the primary suspect based on upstream issue https://github.com/niri-wm/niri/issues/3295 which describes a VRAM/GTT leak when screens are powered off. The leak manifests as shmem on integrated GPUs because they share system RAM.

After this work, the machine should run for weeks without OOM kills from shmem accumulation. The user will verify by monitoring shmem via /proc/meminfo over multi-day sessions after each variable change.

## Progress

- [x] (2026-03-24) Initial investigation: identified shmem/unevictable as root cause of OOM kills
- [x] (2026-03-24) Identified three variables: nixpkgs (mesa), nixos-hardware (surface kernel), niri
- [x] (2026-03-24) Found upstream niri issue #3295 confirming shmem leak when screens power off
- [x] (2026-03-24) Catalogued kernel versions: boot -3 had 6.15.9, boots -2 and 0 have 6.18.8
- [x] (2026-03-24) Catalogued nixos-hardware revs: boot -3 had a351494b (Jan 25), boot -2 had f8e82243 (Mar 15)
- [x] (2026-03-24) Milestone 1: Roll back nixos-hardware to a351494b in flake.nix (flake.nix edited, pending rebuild/reboot/monitor)
- [ ] Milestone 2: If still leaking, roll back niri to the version from boot -3
- [ ] Milestone 3: If still leaking, roll back nixpkgs to fa83fd8

## Surprises & Discoveries

- The "GPU Memory Used: 97%" reported by xpu-smi is misleading on integrated GPUs. It reports total system RAM as GPU memory since the GPU shares it. Not indicative of a GPU-specific problem.
- OOM kill victims are collateral damage. The actual shmem consumer (25 GB) is never among the killed processes, suggesting the memory is held by the kernel/driver on behalf of the compositor, not by a single userspace process.
- Upstream niri issue #3295 confirms: video memory / GTT climbs at ~20 MB/s when displays are powered off. Memory is freed when displays power back on. Does not reproduce on Sway or GNOME Cosmic, pointing to niri specifically.
- Boot -3 (kernel 6.15.9, nixos-hardware a351494b from Jan 25) did not exhibit the shmem leak. The leak appeared after upgrading to kernel 6.18.8 and newer nixos-hardware. This could be a kernel regression, a nixos-hardware surface kernel config change, or a niri update coinciding with the same timeframe.

## Decision Log

- Decision: Roll back nixos-hardware first (Milestone 1), before touching niri or nixpkgs.
  Rationale: The kernel jumped from 6.15.9 to 6.18.8 between the last known-good boot (-3) and the first problematic boot. nixos-hardware provides the surface-specific kernel configuration. Rolling this back is the least disruptive change (single line in flake.nix) and tests the kernel variable in isolation. If the surface kernel config or kernel version is responsible, this fixes it without losing niri or mesa updates.
  Date: 2026-03-24

## Outcomes & Retrospective

(To be filled as milestones complete.)

## Context and Orientation

furfur is a Microsoft Surface Pro with Intel Iris Xe integrated graphics running NixOS 25.11. The key files are:

- `flake.nix` (line 84): defines the nixos-hardware input, currently `github:NixOS/nixos-hardware/master`
- `modules/nixos-hardware.nix`: imports `inputs.nixos-hardware.nixosModules.microsoft-surface-pro-intel` and sets `hardware.microsoft-surface.kernelVersion = "stable"`
- `modules/machines/furfur.nix`: the machine configuration, imports `self.nixosModules.microsoft-surface` and `self.nixosModules.niri`, sets `boot.kernelParams = ["i915.enable_psr=0"]`

The nixos-hardware module provides a surface-patched kernel. The "stable" kernel version setting selects the kernel version that nixos-hardware considers stable for Surface devices. The jump from kernel 6.15.9 to 6.18.8 happened via a nixos-hardware update.

Three variables control the graphics/compositor stack:
1. **nixos-hardware** (surface kernel) — changed from rev a351494b (Jan 25, kernel 6.15.9) to f8e82243 (Mar 15, kernel 6.18.8)
2. **nixpkgs** (mesa, system libraries) — changed from fa83fd8 (Jan 28) to 812b398 (Mar 20)
3. **niri** (Wayland compositor) — version changed between these flake.lock snapshots

The OOM-free boot -3 used nixos-hardware a351494b. We roll back to that first.

## Milestone 1: Roll back surface kernel via nixos-hardware

The goal is to pin nixos-hardware to the last known-good revision (a351494b) and verify that the kernel reverts to 6.15.9 on furfur. This tests whether the kernel upgrade caused the shmem leak.

Edit `flake.nix` line 84. Change:

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

to:

    nixos-hardware.url = "github:NixOS/nixos-hardware?rev=a351494b0e35fd7c0b7a1aae82f0afddf4907aa8";

Then run from /home/binarin/personal-workspace/nixos-config:

    nix flake lock --update-input nixos-hardware

This should update flake.lock to pin nixos-hardware at a351494b. Verify with:

    nix flake metadata | grep nixos-hardware

Build the system without switching yet:

    nixos-rebuild build --flake .#furfur

Verify the built system uses kernel 6.15.x:

    readlink result/kernel

Expected output should contain "linux-6.15" in the store path. If it still shows 6.18, investigate what else provides the kernel.

After confirming the kernel version, switch and reboot:

    sudo nixos-rebuild switch --flake .#furfur
    sudo reboot

After reboot, verify:

    uname -r
    # Expected: 6.15.9 or similar 6.15.x

Monitor shmem over a multi-day session. Check periodically with:

    grep -E "Shmem:|Unevictable:" /proc/meminfo

Lock the screen / let displays power off and check if shmem climbs. If shmem stays under 2 GB during normal use including screen-off periods, this milestone succeeds and the kernel was the issue. If shmem still climbs to 10+ GB, proceed to Milestone 2.

## Milestone 2: Roll back niri

If Milestone 1 does not resolve the leak, the niri compositor is the next suspect per upstream issue #3295. The niri input in flake.nix needs to be identified and pinned to the version from the flake.lock at commit a136273 (Jan 21) or 1ad1917 (Feb 1), whichever was used for boot -3.

Steps: identify the niri flake input, find its rev from git show 1ad1917:flake.lock, pin it in flake.nix, rebuild, reboot, and monitor shmem as in Milestone 1. Detailed commands to be filled in when this milestone is reached.

## Milestone 3: Roll back nixpkgs (mesa)

If Milestones 1 and 2 both fail, the mesa/graphics driver from nixpkgs is the remaining variable. Pin nixpkgs to rev fa83fd8 and rebuild. This is the most disruptive rollback as it affects the entire system. Detailed steps to be filled in if needed.

## Validation and Acceptance

Success means furfur runs for at least 3 days of normal use, including screen lock/power-off cycles, without shmem exceeding 3 GB or any OOM kills occurring. Verify with:

    journalctl -k | grep -i "out of memory"
    journalctl -u systemd-oomd | grep -i kill
    grep -E "Shmem:|Unevictable:" /proc/meminfo

All three should show no new OOM events and shmem/unevictable under 3 GB.

## Idempotence and Recovery

Each milestone is a single flake.nix edit that can be reverted by restoring the previous line. The system can always be rolled back to the current generation via the boot menu (systemd-boot keeps previous generations). No destructive operations are involved.

## Artifacts and Notes

Key store paths for reference:
- Boot -3 (known good): /nix/store/nmkmn28jq3kh6a4sklsk7k65w3xhwh8p-nixos-system-furfur-25.11.20260128.fa83fd8 (kernel 6.15.9)
- Boot -2 (last clean): /nix/store/izrh6lnpzg9100sv60nsd3bpp1a11850-nixos-system-furfur-25.11.20260314.e9f278f (kernel 6.18.8)
- Boot 0 (OOMs): /nix/store/5fihvhd6cpag09q0ak4brcw39l3abssb-nixos-system-furfur-25.11.20260320.812b398 (kernel 6.18.8)

nixos-hardware revisions:
- Known good: a351494b0e35fd7c0b7a1aae82f0afddf4907aa8 (2026-01-25)
- Current: f8e82243fd601afb9f59ad230958bd073795cbfe (2026-03-15)

## Interfaces and Dependencies

The only file edited is `flake.nix`, specifically the `nixos-hardware` input URL on line 84. No code changes, no new modules, no new dependencies. The nixos-hardware module at `modules/nixos-hardware.nix` imports `microsoft-surface-pro-intel` and sets kernel version to "stable", which is interpreted by the nixos-hardware module to select the appropriate kernel.

---

Revision note (2026-03-24): Reworked initial investigation notes into full ExecPlan format per ../PLANS.md. Structured as three sequential rollback milestones (nixos-hardware, niri, nixpkgs) based on user direction. Added upstream niri issue #3295 context. First action is pinning nixos-hardware to a351494b.
