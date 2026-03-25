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
- [x] (2026-03-24) Milestone 1: Roll back nixos-hardware to a351494b in flake.nix (commit cddf4bd, pending rebuild/reboot/monitor)
- [x] (2026-03-24) Enabled Prometheus metrics export on furfur (nixos-config.export-metrics.enable = true) for continuous shmem monitoring via node_memory_Shmem_bytes in VictoriaMetrics/Grafana
- [x] (2026-03-24) Restored rust-1.91-fix kernel patch (rust-fix.patch + boot.kernelPatches in nixos-hardware.nix) — needed for building the older surface kernel from pinned nixos-hardware
- [x] (2026-03-24) Milestone 1 result: reverting kernel to 6.15 did NOT fix the leak (commit 10b7db0)
- [x] (2026-03-24) Identified niri versions: boot -3 had niri v25.11 from nixpkgs (commit 15c52bfb4318), current has niri from git (commit b07bde3ee82d)
- [x] (2026-03-25) Milestone 2 attempt: Reverted niri to v25.11 (15c52bfb4318) — did NOT fix the leak
- [x] (2026-03-25) Tested the complete old system generation (working-furfur, store path w6dqk2vlrnip0gxk3h58k0v3760y3xqj) — NO leak. Identified versions: niri 25.11, mesa 25.2.6, kernel 6.12.67
- [x] (2026-03-25) Key finding: old working system has kernel 6.12.67, NOT 6.15.9 like boot -3. Previous kernel rollback only went to 6.15, not far enough. Mesa version number is same (25.2.6) but built from different nixpkgs (fa83fd8 vs current)
- [x] (2026-03-25) Traced working-furfur system source to nixos-config commit 068a3c3e. It uses nixos-hardware a351494b (same as Milestone 1) and nixpkgs fa83fd8. At that nixos-hardware rev, "stable" = 6.15.9 and "longterm" = 6.12.19. The working system was built from a dirty worktree — likely with kernelVersion changed to "longterm" or equivalent, producing kernel 6.12.67 (a later patch of the 6.12 series).
- [x] (2026-03-25) Milestone 2a: Switch kernelVersion from "stable" to "longterm" in both modules/nixos-hardware.nix and modules/machines/furfur.nix. This selects kernel 6.12.x via nixos-hardware, testing whether the leak is a kernel regression in 6.15+.
- [x] (2026-03-25) Milestone 2a build failure: kernel 6.12 (longterm) doesn't build with current nixos-hardware (master) — wrong patches. Pinned nixos-hardware back to a351494b to get matching surface patches for kernel 6.12.
- [ ] Milestone 2a validation: Rebuild with pinned nixos-hardware + longterm kernel, reboot, monitor shmem during screen-off periods
- [ ] Milestone 3: If kernel 6.12 still leaks, roll back nixpkgs to fa83fd8

## Surprises & Discoveries

- The "GPU Memory Used: 97%" reported by xpu-smi is misleading on integrated GPUs. It reports total system RAM as GPU memory since the GPU shares it. Not indicative of a GPU-specific problem.
- OOM kill victims are collateral damage. The actual shmem consumer (25 GB) is never among the killed processes, suggesting the memory is held by the kernel/driver on behalf of the compositor, not by a single userspace process.
- Upstream niri issue #3295 confirms: video memory / GTT climbs at ~20 MB/s when displays are powered off. Memory is freed when displays power back on. Does not reproduce on Sway or GNOME Cosmic, pointing to niri specifically.
- Boot -3 (kernel 6.15.9, nixos-hardware a351494b from Jan 25) was previously assumed leak-free, but the actual confirmed leak-free system (/nix/store/w6dqk2vlrnip0gxk3h58k0v3760y3xqj-nixos-system-furfur-25.11.20260128.fa83fd8) used kernel 6.12.67. Boot -3 was never monitored long enough to confirm whether it leaked.
- Reverting kernel to 6.15.9 (Milestone 1) did NOT fix the leak. However, this does NOT rule out the kernel — the actual working system used kernel 6.12.67, not 6.15.9. The regression could be between 6.12 and 6.15.
- Boot -3 used niri v25.11 from nixpkgs (tag v25.11, commit 15c52bfb4318f3b2452f511d5367b4bfe6335242). The niri flake input did not exist at that time — it was added later (commit 72a6654). The current leaking build uses niri from git at commit b07bde3ee82dd73115e6b949e4f3f63695da35ea.
- Reverting niri to v25.11 (15c52bfb4318) on the current system did NOT fix the leak. Since the old working system also uses niri v25.11, niri version is ruled out as the sole cause.
- The old working system (working-furfur, /nix/store/w6dqk2vlrnip0gxk3h58k0v3760y3xqj-nixos-system-furfur-25.11.20260128.fa83fd8) confirmed no leak. Its versions: niri 25.11 (r532p9gafrnwxinr8czdklc150ikxwjp), mesa 25.2.6 (2qfbxd5268m5v1h8f8nwz6kp18x3d0dy), kernel 6.12.67 (4c083cypp2xgzf226d2ng7kalgl0fzmc). Mesa has the same version number as current but different store paths, meaning it was built from a different nixpkgs snapshot (fa83fd8) and may have different build-time dependencies or patches.
- The remaining variables are: (1) kernel (6.12 vs 6.15/6.18 — the only confirmed leak-free system used 6.12.67), (2) nixpkgs (fa83fd8 vs current — affects mesa build, libdrm, and all system libs). Niri is ruled out.
- The working-furfur system was built from nixos-config commit 068a3c3e with a dirty worktree. The clean config at that commit sets kernelVersion = "stable" (6.15.9 at nixos-hardware a351494b), but the actual built system has kernel 6.12.67. The dirty change was most likely switching kernelVersion to "longterm" (which gives 6.12.x at that nixos-hardware rev).
- nixos-hardware's kernelVersion option: "stable" selects the latest supported kernel (6.15.9 at a351494b, 6.18.8 at current master), "longterm" selects the LTS kernel (6.12.x). Both modules/nixos-hardware.nix and modules/machines/furfur.nix set this option — both must be changed.
- Kernel 6.12 (longterm) fails to build with current nixos-hardware master — the surface patches don't match. The patches at nixos-hardware a351494b are compatible with the 6.12 longterm kernel. This means testing kernel 6.12 also requires pinning nixos-hardware, combining kernel and nixos-hardware variables in a single change.

## Decision Log

- Decision: Roll back nixos-hardware first (Milestone 1), before touching niri or nixpkgs.
  Rationale: The kernel jumped from 6.15.9 to 6.18.8 between the last known-good boot (-3) and the first problematic boot. nixos-hardware provides the surface-specific kernel configuration. Rolling this back is the least disruptive change (single line in flake.nix) and tests the kernel variable in isolation. If the surface kernel config or kernel version is responsible, this fixes it without losing niri or mesa updates.
  Date: 2026-03-24

- Decision: Milestone 1 failed — kernel rollback did not fix the leak. Proceeding to bisect niri (Milestone 2).
  Rationale: With the kernel ruled out, niri is the strongest suspect per upstream issue #3295. Boot -3 used niri v25.11 from nixpkgs (commit 15c52bfb4318). The current build uses niri from git (commit b07bde3ee82d). A git bisect between these two commits will pinpoint the regression.
  Date: 2026-03-24

- Decision: Niri is ruled out. Milestone 2 (niri bisect) is no longer needed.
  Rationale: Reverting niri to v25.11 (the exact same version as the working system) did not fix the leak. Niri version is the same in both working and broken systems.
  Date: 2026-03-25

- Decision: Test kernel 6.12 before rolling back nixpkgs (Milestone 2a). Changed kernelVersion from "stable" to "longterm" in both modules/nixos-hardware.nix and modules/machines/furfur.nix.
  Rationale: The only confirmed leak-free system (working-furfur) used kernel 6.12.67. The previous kernel rollback (Milestone 1) only went to 6.15.9, which still leaked. The working-furfur source (nixos-config commit 068a3c3e) was built from a dirty worktree that likely had kernelVersion set to "longterm". Testing 6.12 is a single config change and isolates the kernel variable before the more disruptive nixpkgs rollback.
  Date: 2026-03-25

- Decision: Pin nixos-hardware to a351494b alongside longterm kernel, since current nixos-hardware master's surface patches don't build with kernel 6.12.
  Rationale: The longterm kernel 6.12.x fails to build with nixos-hardware master — patch mismatch. The a351494b rev has compatible patches for 6.12. This combines two variables (kernel + nixos-hardware) in one change, but it's the same combination as the confirmed working system (working-furfur). If this works, Milestone 3 can further isolate by testing current nixos-hardware with kernel 6.12 patches fixed separately.
  Date: 2026-03-25

## Outcomes & Retrospective

(To be filled as milestones complete.)

## Context and Orientation

furfur is a Microsoft Surface Pro with Intel Iris Xe integrated graphics running NixOS 25.11. The key files are:

- `flake.nix` (line 84): defines the nixos-hardware input, currently `github:NixOS/nixos-hardware/master`
- `modules/nixos-hardware.nix`: imports `inputs.nixos-hardware.nixosModules.microsoft-surface-pro-intel` and sets `hardware.microsoft-surface.kernelVersion` (now "longterm", previously "stable")
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

## Milestone 2: Niri bisect (SKIPPED — niri ruled out)

Reverting niri to v25.11 (15c52bfb4318) did not fix the leak, and the confirmed working system also uses niri v25.11. Niri version is not the cause. This milestone is skipped.

## Milestone 2a: Switch to kernel 6.12 (longterm)

The only confirmed leak-free system used kernel 6.12.67. The previous kernel test (Milestone 1) only went back to 6.15.9, which still leaked. The regression may be between 6.12 and 6.15.

Edit both `modules/nixos-hardware.nix` and `modules/machines/furfur.nix`. Change:

    hardware.microsoft-surface.kernelVersion = "stable";

to:

    hardware.microsoft-surface.kernelVersion = "longterm";

At the current nixos-hardware rev, "longterm" selects kernel 6.12.x with surface patches. Rebuild and reboot:

    sudo nixos-rebuild switch --flake .#furfur
    sudo reboot

After reboot, verify:

    uname -r
    # Expected: 6.12.x

Monitor shmem during screen-off periods:

    grep -E "Shmem:|Unevictable:" /proc/meminfo

If shmem stays under 3 GB during screen-off, the kernel 6.15+ regression is confirmed. If it still leaks, proceed to Milestone 3 (nixpkgs rollback).

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
- Working-furfur (confirmed no leak): /nix/store/w6dqk2vlrnip0gxk3h58k0v3760y3xqj-nixos-system-furfur-25.11.20260128.fa83fd8 (kernel 6.12.67, niri 25.11, mesa 25.2.6)
- Boot -3 (known good): /nix/store/nmkmn28jq3kh6a4sklsk7k65w3xhwh8p-nixos-system-furfur-25.11.20260128.fa83fd8 (kernel 6.15.9)
- Boot -2 (last clean): /nix/store/izrh6lnpzg9100sv60nsd3bpp1a11850-nixos-system-furfur-25.11.20260314.e9f278f (kernel 6.18.8)
- Boot 0 (OOMs): /nix/store/5fihvhd6cpag09q0ak4brcw39l3abssb-nixos-system-furfur-25.11.20260320.812b398 (kernel 6.18.8)

nixos-hardware revisions:
- Known good: a351494b0e35fd7c0b7a1aae82f0afddf4907aa8 (2026-01-25)
- Current: f8e82243fd601afb9f59ad230958bd073795cbfe (2026-03-15)

## Interfaces and Dependencies

Files edited: `modules/nixos-hardware.nix` and `modules/machines/furfur.nix` (kernelVersion "stable" → "longterm"), and previously `flake.nix` (nixos-hardware input pin). The nixos-hardware module at `modules/nixos-hardware.nix` imports `microsoft-surface-pro-intel` and sets kernel version to "longterm", which selects the LTS 6.12.x kernel.

niri versions in store for reference:
- Boot -3 niri: /nix/store/r532p9gafrnwxinr8czdklc150ikxwjp-niri-25.11 (from nixpkgs, version "niri 25.11 (Nixpkgs)", git tag v25.11 = 15c52bfb4318)
- Current niri: /nix/store/9aqh3cnnsrx3di9nqphspqwb7sqp28bz-niri-b07bde3 (from niri flake input, git commit b07bde3ee82d)

---

Revision note (2026-03-24): Reworked initial investigation notes into full ExecPlan format per ../PLANS.md. Structured as three sequential rollback milestones (nixos-hardware, niri, nixpkgs) based on user direction. Added upstream niri issue #3295 context. First action is pinning nixos-hardware to a351494b.

Revision note (2026-03-24): Milestone 1 failed — kernel rollback to 6.15.9 did not resolve the shmem leak. Updated plan to proceed with Milestone 2: bisecting niri between v25.11 (15c52bfb4318, known good from boot -3) and b07bde3ee82d (known bad, current git HEAD). Discovered that boot -3 did not use a niri flake input at all — it used niri v25.11 from nixpkgs. The niri flake input was added later in commit 72a6654.

Revision note (2026-03-25): Both niri revert (to v25.11 / 15c52bf) and kernel revert (to 6.15) failed individually. However, booting the complete old system generation (working-furfur) confirmed no leak. That system uses: niri 25.11, mesa 25.2.6, kernel 6.12.67. Critical new finding: the old working system has kernel 6.12.67, not 6.15.9. The previous kernel rollback only went to 6.15 which was not far enough. Niri is now ruled out (same version in both working and broken systems). Remaining suspects: nixpkgs (mesa/libdrm build differences despite same version number) and kernel (6.12 vs 6.18, with 6.15 already excluded).

Revision note (2026-03-25): Traced working-furfur to nixos-config commit 068a3c3e (dirty worktree build). Same nixos-hardware (a351494b) and nixpkgs (fa83fd8) as boot -3, but kernel 6.12.67 instead of 6.15.9 — the dirty change was likely switching kernelVersion to "longterm". Added Milestone 2a: switch kernelVersion from "stable" to "longterm" in modules/nixos-hardware.nix and modules/machines/furfur.nix. Skipped Milestone 2 (niri bisect) since niri is ruled out. Milestone 3 (nixpkgs rollback) remains as fallback if kernel 6.12 still leaks.
