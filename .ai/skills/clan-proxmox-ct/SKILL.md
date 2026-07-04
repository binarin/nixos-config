---
name: clan-proxmox-ct
description: Use when creating a new NixOS Proxmox LXC container machine in a clan-based nix-config, or when asked to add a new container/host/CT/VM to the clan inventory
---

# Clan Proxmox CT

## Overview

Add a new bare-minimum Proxmox LXC container to a clan nix-config. Three deliverables: machine module, IP allocation, git tracking.

## When to Use

- Creating a new Proxmox CT machine
- Setting up a new clan host from scratch
- Asked to add a new container/host/CT to the config

**Not for:** VMs (use `qemu-guest` + disko, not `lxc`), physical machines, non-clan configs.

## Steps

### 1. Create machine module

Create `modules/machines/<name>.nix` from this template (no services, no extra inputs):

```nix
{
  self,
  config,
  lib,
  inputs,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.<name> = {
    hostname = "<name>";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.<name>;
    };
  };

  clan.inventory.machines.<name> = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.<name>.home.primary.address;
  };

  clan.machines.<name> = {
    imports = [
      self.nixosModules.<name>-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.<name> = lib.mkForce (
    self.clan.nixosConfigurations.<name>.extendModules {
      specialArgs.inventoryHostName = "<name>";
    }
  );

  flake.nixosModules.<name>-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.<name>-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      nixos-config.export-metrics.enable = false;
    };
}
```

**Imports:** `baseline` + `lxc` (not `qemu-guest` — that's for VMs). No extra flake inputs needed for bare-minimum.

### 2. Allocate IP

Add to `inventory/networks/home.toml` in the `[ipam]` section. Pick the next unused sequential IP (use `# unallocated` comments as guide). Use the hostname-form format with MAC:

```toml
"192.168.2.<N>" = { hostname = "<name>", mac = "02:00:<host-id-bytes>" }
```

**MAC/host-id relationship:** The MAC determines the host-id. Host-id = last 4 bytes of MAC (lowercase hex, no separators). MAC format is always `02:00:XX:XX:XX:XX`.

**Order matters:** Set a placeholder MAC first, then `clan vars generate <name> --generator hostId --no-regenerate` will produce the host-id. Derive the final MAC from the generated host-id and update the toml.

Or: set MAC with known host-id bytes directly:
1. Pick 4 random hex bytes (e.g. `16:59:43:5d`)
2. MAC = `02:00:16:59:43:5d`
3. host-id will be `1659435d`

### 3. Git track

```bash
git add --intent-to-add modules/machines/<name>.nix
```

The machine module must be known to git for nix/flake-file to discover it. `--intent-to-add` is sufficient — content sync is not required.

### 4. Generate host-id

```bash
clan vars generate <name> --generator hostId --no-regenerate
```

Generate only the host-id var. Do **not** run `clan vars generate <name>` without `--generator` — other vars (openssh, tailscale, passwords) may require interactive user input and should be generated separately by the user.

### 5. Verify MAC

Check the generated host-id and ensure the MAC in home.toml matches:

```bash
cat vars/per-machine/<name>/hostId/hostId/value
```

The MAC's last 4 bytes (lowercase, no colons) must equal this value. Update `home.toml` if they differ.

### 6. Verify configuration evals

```bash
nix eval .#nixosConfigurations.<name>.config.system.build.toplevel
```

This confirms the module, IP allocation, and host-id are wired correctly. If it fails, check:
- Machine module is `git add --intent-to-add`'d
- IP/MAC entry exists in `home.toml` with correct hostname
- `clan vars generate` ran successfully

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Using `qemu-guest` instead of `lxc` | CTs use `lxc`, VMs use `qemu-guest` + disko |
| Setting MAC before host-id exists | Either set known MAC bytes first, or update MAC after `clan vars generate` |
| MAC/host-id mismatch | host-id = last 4 bytes of MAC, lowercase, no separators. Verify with step 5 |
| Forgetting `git add --intent-to-add` | Nix won't see the module file until git knows about it |
| Wrong file location | Machine modules go in `modules/machines/`, not `machines/` (those are hardware-config/disko) |
| Adding flake inputs for bare-minimum | Not needed — only add inputs if the machine uses external flakes (like niks3) |

## Machine File Location

- **Machine modules:** `modules/machines/<name>.nix` — clan machine definition
- **Hardware configs:** `machines/<name>/hardware-configuration.nix` — not needed for CTs
- **Disko:** `machines/<name>/disko.nix` — not needed for CTs
