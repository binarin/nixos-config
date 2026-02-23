---
name: add-nixos-machine
description: Guides the process of adding new NixOS machine configurations following the dendritic pattern. Use when creating new NixOS machines (qemu-guest, lxc, bare-metal). Handles inventory setup, hostId generation, and module creation.
---

# Add NixOS Machine

This skill guides you through adding a new NixOS machine configuration following the dendritic pattern used in this repository.

## Workflow Checklist

Copy and complete this checklist when adding a new machine:

- [ ] **1. Pre-check**: Run `nix fmt && just lint && just eval-all` to ensure the configuration is clean before making changes
- [ ] **2. Determine machine type**: qemu-guest, lxc, or bare-metal
- [ ] **3. Choose machine name**: Should be unique and follow existing naming conventions
- [ ] **4. Generate hostId**: Run `./scripts/generate-hostid.sh` from skill directory
- [ ] **5. Add hostId to inventory**: Edit `inventory/host-id.nix`
- [ ] **6. Allocate IP** (if needed): Edit `inventory/networks/home.nix` or skip for microvm/isolated machines
- [ ] **7. Create machine module**: Create `modules/machines/<machine-name>.nix` using examples from REFERENCE.md
- [ ] **8. Stage new file**: `git add modules/machines/<machine-name>.nix`
- [ ] **9. Run validation**: `just nixOpts= eval-nixos` in a loop until clean
- [ ] **10. Get stateVersion**: Run the get-state-version script and add explicit value
- [ ] **11. Run comprehensive validation**: `just eval-all`
- [ ] **12. Format and lint**: `nix fmt && just lint`
- [ ] **13. Commit**: Follow git workflow in CLAUDE.md

## Machine Types

### qemu-guest
Standard VM running in QEMU/KVM. Uses `modulesPath + "/profiles/qemu-guest.nix"`.

**Template**: See REFERENCE.md for complete examples.

### lxc
Proxmox LXC container. Imports `self.nixosModules.lxc`.

**Typical imports:**
```nix
imports = [
  self.nixosModules.default
  self.nixosModules.lxc
  self.nixosModules.binarin-baseline
];
```

### bare-metal
Physical machine requiring hardware-configuration.nix.

**Typical imports:**
```nix
imports = [
  self.nixosModules.baseline
  (modulesPath + "/installer/scan/not-detected.nix")
  "${self}/machines/<machine-name>/hardware-configuration.nix"
  # ... other modules
];
```

## Helper Scripts

### generate-hostid.sh
Generates a random 8-character hex hostId:
```bash
files/claude-skills/add-nixos-machine/scripts/generate-hostid.sh
```

### find-free-ip.sh
Finds the first available IP in a network:
```bash
files/claude-skills/add-nixos-machine/scripts/find-free-ip.sh [network]
# Default network: home
```

### get-state-version.sh
Gets the default stateVersion from an existing configuration:
```bash
files/claude-skills/add-nixos-machine/scripts/get-state-version.sh <machine-name>
```

**Workflow for stateVersion:**
1. Create machine config WITHOUT explicit stateVersion
2. Run validation to ensure config evaluates
3. Run `get-state-version.sh <machine-name>` to get the default value
4. Add explicit `system.stateVersion = "X.Y";` to the config

## Key Patterns

### inventoryHostName in specialArgs
For machines that use inventory IP allocation, pass `inventoryHostName` in specialArgs:

```nix
flake.nixosConfigurations.machine-name = inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = {
    inventoryHostName = "machine-name";
  };
  modules = [ self.nixosModules.machine-name-configuration ];
};
```

### Machines without inventory IP
For isolated machines, skip the inventory IP allocation but still:
- Add hostId to `inventory/host-id.nix`
- Configure networking manually in the module

### Per-machine user configuration
To add per-machine home-manager settings:

```nix
flake.homeModules.machine-name-binarin = {...}: {
  key = "nixos-config.modules.home.machine-name-binarin";
  # per-machine settings here
};

# In the nixosModule:
home-manager.users.binarin = self.homeModules.machine-name-binarin;
```

## Quick Reference

**File locations:**
- Machine modules: `modules/machines/<name>.nix`
- Host IDs: `inventory/host-id.nix`
- IP allocation: `inventory/networks/home.nix`
- Hardware configs: `machines/<name>/hardware-configuration.nix`

**Validation commands:**
- Fast: `just nixOpts= eval-nixos`
- Comprehensive: `just eval-all`
- Format: `nix fmt && just lint`

**Common module imports:**
- `self.nixosModules.default` - base configuration
- `self.nixosModules.baseline` - baseline for workstations/servers
- `self.nixosModules.srvos-bits` - server-oriented configuration
- `self.nixosModules.lxc` - LXC container support
- `self.nixosModules.binarin-workstation` - user binarin with full workstation setup
- `self.nixosModules.binarin-baseline` - user binarin with minimal setup
- `self.nixosModules.tailscale` - Tailscale VPN

For complete module templates, see [REFERENCE.md](REFERENCE.md).
