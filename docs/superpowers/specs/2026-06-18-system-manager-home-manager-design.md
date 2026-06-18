# System-Manager Home-Manager Integration

## Goal

Integrate home-manager into system-manager the same way the NixOS module does: declare `home-manager.users.<name>` inside the system-manager config, with per-user activation via system-level systemd services triggered during `system-manager activate`.

This eliminates the need for separate deploy-rs profiles for HM and removes fake `osConfig` injection.

## Architecture

### New system-manager module: `system-manager-home-manager`

Exposed as `flake.systemModules.home-manager` from `modules/system-manager-home-manager.nix`.

#### Options

| Option | Type | Description |
|--------|------|-------------|
| `home-manager.users` | `attrsOf hmModule` | Per-user HM configuration |
| `home-manager.useGlobalPkgs` | `bool` (default `false`) | Use system-manager's `pkgs` in HM |
| `home-manager.sharedModules` | `listOf raw` | Modules added to all users |
| `home-manager.extraSpecialArgs` | `attrs` | Extra args passed to HM evaluation |
| `home-manager.backupFileExtension` | `nullOr str` | Passed as `HOME_MANAGER_BACKUP_EXT` env var |

#### HM evaluation

Each user's config is evaluated via `lib.evalModules` importing home-manager's `modules/modules.nix` (same mechanism as `nixos/common.nix`). The submodule type uses:

```nix
specialArgs = {
  lib = extendedLib;  # HM's stdlib-extended
  osConfig = config;  # the system-manager config
  systemManagerConfig = config;  # explicit alias
  osClass = null;  # not NixOS, not darwin
  modulesPath = toString ../modules;  # HM modules path
} // cfg.extraSpecialArgs;
```

#### Auto-wiring from `users.users`

For each `home-manager.users.<name>`:
- `home.username` ← `config.users.users.${name}.name`
- `home.homeDirectory` ← `config.users.users.${name}.home`
- `home.uid` ← `config.users.users.${name}.uid` (when non-null)

#### Systemd service generation

For each user, generates `systemd.services.home-manager-<username>`:

```
Type = "oneshot"
RemainAfterExit = "yes"
TimeoutStartSec = "5m"
SyslogIdentifier = "hm-activate-<username>"
User = <username>
wantedBy = ["system-manager.target"]
wants = ["nix-daemon.socket"]
after = ["nix-daemon.socket"]
```

`ExecStart` runs a shell wrapper that:
1. Sources login environment (so HM activation has a sane PATH)
2. Optionally imports systemd user session variables (DISPLAY, WAYLAND_DISPLAY, etc.) if the user is logged in
3. Runs `${activationPackage}/activate`

Environment variables set on the service:
- `HOME_MANAGER_BACKUP_EXT` when `backupFileExtension` is non-null

### New system-manager module: `system-manager-compat`

Exposed as `flake.systemModules.compat` from `modules/system-manager-compat.nix`. Defines options that HM modules in this repo reference via `osConfig`:

| Option | Type | Default |
|--------|------|---------|
| `services.graphical-desktop.enable` | `bool` | `false` |
| `impermanence.enable` | `bool` | `false` |
| `networking.hostName` | `str` | required |

### Changes to `modules/machines/murmur.nix`

- Remove `flake.homeConfigurations.murmur` (standalone HM config)
- Remove `flake.deploy.nodes.murmur.profiles.home`
- Add `self.systemModules.home-manager` and `self.systemModules.compat` to the `makeSystemConfig` modules list
- Move HM config inline as `home-manager.users.allebedev = ...` inside system-manager config
- Set `services.graphical-desktop.enable = true` and `networking.hostName = "murmur"` as real system-manager options
- Remove fake `extraSpecialArgs.osConfig.*` injection

## Activation flow

```
deploy-rs: system-manager activate
  → systemctl daemon-reload
  → system-manager.target starts
    → home-manager-allebedev.service starts (WantedBy)
      → runs as user allebedev
      → sources login env
      → ${activationPackage}/activate
      → HM generation activated
```

## Scope boundaries

**In scope:**
- `home-manager.users`, `useGlobalPkgs`, `sharedModules`, `extraSpecialArgs`, `backupFileExtension`
- System-level oneshot service per user
- Compat options for `osConfig` access

**Out of scope (not needed now):**
- `useUserPackages` (installing HM packages via `users.users.<name>.packages`)
- `startAsUserService` (user-level systemd service activation)
- `verbose` option
- `enableLegacyProfileManagement`
- Warnings/assertions forwarding from HM to system-manager (can add later if needed)

## Dependencies

- home-manager source at `inputs.home-manager` (already a flake input)
- system-manager's `users.users` option (already available from upstream nixpkgs module)
- system-manager's `systemd.services` (already available)
