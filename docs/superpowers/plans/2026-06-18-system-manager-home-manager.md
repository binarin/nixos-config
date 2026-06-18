# System-Manager Home-Manager Integration — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Enable `home-manager.users.<name>` inside system-manager configs, with per-user activation via system-level systemd oneshot services — mirroring the NixOS HM integration.

**Architecture:** A new `systemModules.home-manager` evaluates per-user HM configs using the home-manager module system, wires `home.username`/`home.homeDirectory` from `users.users`, and generates `systemd.services.home-manager-<user>` oneshot units. A small `systemModules.compat` provides the `osConfig.*` options that existing HM modules reference.

**Tech Stack:** Nix module system, home-manager `modules/modules.nix`, system-manager `systemd.services`

---

## File Structure

| File | Role |
|------|------|
| `modules/system-manager-home-manager.nix` | **Create** — flake-parts module exposing `flake.systemModules.home-manager` |
| `modules/system-manager-compat.nix` | **Create** — flake-parts module exposing `flake.systemModules.compat` |
| `modules/machines/murmur.nix` | **Modify** — integrate HM into system-manager config, remove standalone HM config |
| `modules/bubuntu.nix` | **Modify** — include compat module (since all bubuntu machines will need the compat options) |

---

### Task 1: Create the compat module

**Files:**
- Create: `modules/system-manager-compat.nix`

This module defines the compatibility options that existing HM modules in this repo reference via `osConfig`.

- [ ] **Step 1: Create `modules/system-manager-compat.nix`**

```nix
{ self, ... }:
{
  flake.systemModules.compat =
    { lib, ... }:
    {
      options = {
        services.graphical-desktop.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether this machine runs a graphical desktop session.";
        };

        impermanence.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether impermanence is enabled (always false on system-manager).";
        };

        networking.hostName = lib.mkOption {
          type = lib.types.str;
          description = "The hostname of this machine.";
        };
      };
    };
}
```

- [ ] **Step 2: Verify it evaluates**

Run: `nix eval .#systemModules.compat --apply 'x: builtins.typeOf x'`
Expected: `"set"` (confirms the module is reachable)

- [ ] **Step 3: Commit**

```bash
git add modules/system-manager-compat.nix
git commit -m "feat(system-manager): add compat module for osConfig options"
```

---

### Task 2: Create the home-manager integration module

**Files:**
- Create: `modules/system-manager-home-manager.nix`

This is the main module that evaluates per-user HM configs and generates systemd activation services.

- [ ] **Step 1: Create `modules/system-manager-home-manager.nix`**

```nix
{ self, inputs, ... }:
{
  flake.systemModules.home-manager =
    {
      config,
      lib,
      pkgs,
      utils,
      ...
    }:
    let
      cfg = config.home-manager;

      extendedLib = import "${inputs.home-manager}/modules/lib/stdlib-extended.nix" lib;

      hmModule = lib.types.submoduleWith {
        description = "Home Manager module";
        class = "homeManager";
        specialArgs = {
          lib = extendedLib;
          osConfig = config;
          systemManagerConfig = config;
          osClass = null;
          modulesPath = toString "${inputs.home-manager}/modules";
        } // cfg.extraSpecialArgs;

        modules = [
          ({ name, ... }: {
            imports =
              import "${inputs.home-manager}/modules/modules.nix" {
                inherit pkgs;
                lib = extendedLib;
                useNixpkgsModule = !cfg.useGlobalPkgs;
              }
              ++ cfg.sharedModules;

            config = {
              submoduleSupport.enable = true;

              home = {
                username = config.users.users.${name}.name;
                homeDirectory = config.users.users.${name}.home;
              } // lib.optionalAttrs (config.users.users.${name}.uid != null) {
                uid = config.users.users.${name}.uid;
              };

              nix.package = lib.mkDefault pkgs.nix;
            };
          })
        ];
      };
    in
    {
      options.home-manager = {
        users = lib.mkOption {
          type = lib.types.attrsOf hmModule;
          default = { };
          description = "Per-user Home Manager configuration.";
        };

        useGlobalPkgs = lib.mkEnableOption ''
          using the system configuration's `pkgs` argument in Home Manager.
          This disables the Home Manager options `nixpkgs.*`'';

        sharedModules = lib.mkOption {
          type = with lib.types; listOf raw;
          default = [ ];
          description = "Extra modules added to all users.";
        };

        extraSpecialArgs = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          description = "Extra `specialArgs` passed to Home Manager.";
        };

        backupFileExtension = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            On activation move existing files by appending the given
            file extension rather than exiting with an error.
          '';
        };
      };

      config = lib.mkIf (cfg.users != { }) {
        systemd.services = lib.mapAttrs' (
          _: usercfg:
          let
            inherit (usercfg.home) username homeDirectory activationPackage;
          in
          lib.nameValuePair "home-manager-${utils.escapeSystemdPath username}" {
            wantedBy = [ "system-manager.target" ];
            wants = [ "nix-daemon.socket" ];
            after = [ "nix-daemon.socket" ];

            unitConfig.RequiresMountsFor = homeDirectory;

            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = "yes";
              TimeoutStartSec = "5m";
              SyslogIdentifier = "hm-activate-${username}";
              User = username;
              ExecStart = let
                systemctl = "XDG_RUNTIME_DIR=\${XDG_RUNTIME_DIR:-/run/user/$UID} systemctl";
                sed = "${pkgs.gnused}/bin/sed";
                exportedSystemdVariables = lib.concatStringsSep "|" [
                  "DBUS_SESSION_BUS_ADDRESS"
                  "DISPLAY"
                  "WAYLAND_DISPLAY"
                  "XAUTHORITY"
                  "XDG_RUNTIME_DIR"
                ];
                setupEnv = pkgs.writeScript "hm-setup-env" ''
                  #! ${pkgs.runtimeShell} -el

                  # The activation script is run by a login shell to make sure
                  # that the user is given a sane environment.
                  # If the user is logged in, import variables from their current
                  # session environment.
                  eval "$(
                    ${systemctl} --user show-environment 2> /dev/null \
                    | ${sed} -En '/^(${exportedSystemdVariables})=/s/^/export /p'
                  )"

                  exec "$1/activate"
                '';
              in
              "${setupEnv} ${activationPackage}";
            };

            environment = lib.mkMerge [
              { QT_QPA_PLATFORM = "offscreen"; }
              (lib.mkIf (cfg.backupFileExtension != null) {
                HOME_MANAGER_BACKUP_EXT = cfg.backupFileExtension;
              })
            ];
          }
        ) cfg.users;
      };
    };
}
```

- [ ] **Step 2: Verify module can be referenced**

Run: `nix eval .#systemModules.home-manager --apply 'x: builtins.typeOf x'`
Expected: `"set"`

- [ ] **Step 3: Commit**

```bash
git add modules/system-manager-home-manager.nix
git commit -m "feat(system-manager): add home-manager integration module"
```

---

### Task 3: Include compat in bubuntu base

**Files:**
- Modify: `modules/bubuntu.nix`

Add `self.systemModules.compat` to the imports in bubuntu so all system-manager machines get the compat options.

- [ ] **Step 1: Add compat import to bubuntu**

In `modules/bubuntu.nix`, add `self.systemModules.compat` to the imports list (after existing `self.systemModules.*` entries):

```nix
      imports =
        [
          self.systemModules.sysctl
          self.systemModules.nix-path
          self.systemModules.logind
          self.systemModules.uwsm
          self.systemModules.apt-packages
          self.systemModules.chrome-policies
          self.systemModules.compat          # <-- add this
          self.modules.generic.chrome-policies
          ...
```

- [ ] **Step 2: Commit**

```bash
git add modules/bubuntu.nix
git commit -m "feat(system-manager): include compat module in bubuntu base"
```

---

### Task 4: Migrate murmur to integrated home-manager

**Files:**
- Modify: `modules/machines/murmur.nix`

This is the main integration step. Move the HM config from a standalone `homeConfigurations.murmur` into `home-manager.users.allebedev` inside the system-manager config.

- [ ] **Step 1: Add `self.systemModules.home-manager` to murmur's makeSystemConfig modules**

In the `makeSystemConfig { ... modules = [ ... ] }` call, add:
```nix
self.systemModules.home-manager
```

- [ ] **Step 2: Add home-manager options to the system-manager config**

Inside the system-manager module (the anonymous module in the `modules` list), add:

```nix
home-manager.useGlobalPkgs = true;
home-manager.backupFileExtension = "backup";
home-manager.sharedModules = [
  self.homeModules.home-misc
];
home-manager.extraSpecialArgs = {
  self'.packages = self.packages.x86_64-linux;
  inputs' =
    with lib;
    pipe inputs [
      (filterAttrs (_: v: v ? packages))
      (mapAttrs (
        _: v: {
          packages = v.packages.x86_64-linux;
        }
      ))
    ];
};
```

- [ ] **Step 3: Add `home-manager.users.allebedev` config**

Move the content from the current `flake.homeModules.murmur-home-allebedev` module into:

```nix
home-manager.users.allebedev = self.homeModules.murmur-home-allebedev;
```

Note: Keep `flake.homeModules.murmur-home-allebedev` as a module definition (it's reusable), just reference it from inside the system-manager config.

- [ ] **Step 4: Set compat options in the system-manager config**

Inside the system-manager modules, add:
```nix
services.graphical-desktop.enable = true;
networking.hostName = "murmur";
```

- [ ] **Step 5: Remove standalone homeConfigurations.murmur**

Delete the entire `flake.homeConfigurations.murmur = inputs.home-manager.lib.homeManagerConfiguration { ... };` block.

- [ ] **Step 6: Remove deploy.nodes.murmur.profiles.home**

Change the deploy node from:
```nix
flake.deploy.nodes.murmur = {
  hostname = "murmur";
  sshUser = "allebedev";
  profiles.home = {
    path = self.lib.deploy-home-manager self.homeConfigurations.murmur;
  };
  profiles.system = {
    user = "root";
    path = self.lib.deploy-system-manager self.systemConfigs.murmur;
  };
};
```

To:
```nix
flake.deploy.nodes.murmur = {
  hostname = "murmur";
  sshUser = "allebedev";
  profiles.system = {
    user = "root";
    path = self.lib.deploy-system-manager self.systemConfigs.murmur;
  };
};
```

- [ ] **Step 7: Remove fake osConfig from murmur-home-allebedev**

In `flake.homeModules.murmur-home-allebedev`, the module function arguments currently don't reference `osConfig` directly (they use `inputs'` etc.), but the `extraSpecialArgs` in the old standalone config injected fake values. Since `osConfig` is now the real system-manager config, nothing to change in the module itself — just verify it doesn't reference `osConfig.services.graphical-desktop.enable` or similar (it doesn't; `murmur-home-allebedev` doesn't import modules that use these — the desktop-essentials module doesn't reference osConfig).

- [ ] **Step 8: Build to verify**

Run: `nix build .#systemConfigs.murmur --no-link --print-out-paths`
Expected: Successful build producing a store path. Verify the output contains the HM activation service:
```bash
nix build .#systemConfigs.murmur --no-link --print-out-paths | xargs -I{} cat {}/services.json | jq 'keys' | grep home-manager
```

- [ ] **Step 9: Inspect the generated service unit**

Run:
```bash
path=$(nix build .#systemConfigs.murmur --no-link --print-out-paths)
cat $(jq -r '.["home-manager-allebedev.service"].storePath' $path/services.json)
```

Verify it contains:
- `Type=oneshot`
- `User=allebedev`
- `ExecStart=` pointing to the hm-setup-env wrapper
- `Environment=` containing `HOME_MANAGER_BACKUP_EXT=backup`

- [ ] **Step 10: Commit**

```bash
git add modules/machines/murmur.nix modules/bubuntu.nix
git commit -m "feat(murmur): integrate home-manager into system-manager config

Replaces the standalone homeConfigurations.murmur and separate
deploy-rs home profile with home-manager.users.allebedev declared
inside the system-manager config. Activation now happens via a
system-level systemd service during system-manager activate."
```

---

### Task 5: Verify sops HM module compatibility

**Files:**
- Verify: `modules/sops.nix` (the homeModules.sops)

The `sops` home module references `osConfig.sops.secrets.user-binarin-age.path or null`, `osConfig.impermanence.enable`, and `osConfig.networking.hostName`. Since murmur's system-manager config includes the sops-nix upstream module and our compat module, verify these paths exist.

- [ ] **Step 1: Check if murmur-home-allebedev imports the sops HM module**

Look at `modules/machines/murmur.nix` — the `murmur-home-allebedev` module's imports list. If it does NOT import `self.homeModules.sops`, this task is a no-op. If it does, continue.

Looking at the current imports in `murmur-home-allebedev`:
```
self.homeModules.emacs
self.homeModules.foot
self.homeModules.firefox
self.homeModules.niri
self.homeModules.direnv
self.homeModules.xdg-autostart
self.homeModules.binarin-baseline
self.homeModules.binarin-ssh
self.homeModules.standalone-home-manager-zsh
self.homeModules.desktop-essentials
```

It does NOT import `self.homeModules.sops`, so this is a **no-op**.

- [ ] **Step 2: Verify build still passes (already done in Task 4 Step 8)**

No additional action needed.

- [ ] **Step 3: Commit (no-op — nothing to change)**

---

### Task 6: Clean up unused `home-manager.flakeModules.home-manager` import in murmur

**Files:**
- Modify: `modules/machines/murmur.nix`

The current murmur.nix has `imports = [ inputs.home-manager.flakeModules.home-manager ]` which was needed for `flake.homeConfigurations`. This registers flake-parts support for `homeConfigurations` output. If it's still needed by other machines (it is — imported in `modules/home-manager.nix` for NixOS machines), we can leave the import. But if murmur was the only user, remove it.

- [ ] **Step 1: Check if the import is still needed**

Run: `grep -r "flakeModules.home-manager\|homeConfigurations" modules/ --include="*.nix" | grep -v murmur`

If other files reference `flake.homeConfigurations.*` (like `b-dev-kvm`), the flakeModules import is still needed somewhere. In that case: the import in murmur can be removed since `modules/home-manager.nix` already imports it.

- [ ] **Step 2: Remove the import from murmur.nix if redundant**

Remove from murmur.nix:
```nix
imports = [
  inputs.home-manager.flakeModules.home-manager
];
```

- [ ] **Step 3: Verify build**

Run: `nix build .#systemConfigs.murmur --no-link`
And: `nix build .#homeConfigurations.b-dev-kvm.activationPackage --no-link`
Expected: Both succeed (proving the flakeModule is still loaded from elsewhere).

- [ ] **Step 4: Commit**

```bash
git add modules/machines/murmur.nix
git commit -m "refactor(murmur): remove redundant home-manager flakeModules import"
```
