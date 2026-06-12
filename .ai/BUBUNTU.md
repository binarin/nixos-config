# Bubuntu: System-Manager for Non-NixOS Ubuntu Machines

## What is bubuntu

`flake.systemModules.bubuntu` is the baseline system-manager module for managing Ubuntu (non-NixOS) machines declaratively using numtide/system-manager. It replaces the upstream `nixpkgs/default.nix` stubs module to allow proper option declarations (like `boot.kernel.sysctl`).

Machine: murmur (Ubuntu 24.04 laptop, used as a work desktop with niri/Wayland).

## Architecture Decisions

### Module system infrastructure

- `modules/system-modules.nix` declares `flake.systemModules` and `flake.systemConfigs` as typed flake-parts options (like home-manager's `homeModules`/`homeConfigurations`).
- `systemModules` use `types.deferredModule` — same pattern as NixOS/home-manager modules.
- `systemConfigs` is `types.lazyAttrsOf types.raw` — holds the `makeSystemConfig` outputs.

### bubuntu baseline replaces upstream stubs

- Disables `${inputs.system-manager}/nix/modules/upstream/nixpkgs` (the directory-level module).
- Re-imports individual sub-modules from system-manager (firewall, nix, security-wrappers, openssh, etc.).
- Declares stub options (`system.activationScripts.users`, `fonts.fontconfig.enable`, etc.) for NixOS module compatibility.
- Declares `config.lib` option (needed for `modules.generic.public-keys` to work in system-manager context).
- Disables nginx (not used).

### Option naming: match NixOS where possible

- `boot.kernel.sysctl` — same as NixOS
- `services.logind.settings.Login` / `services.logind.lidSwitch*` — same as NixOS
- `services.openssh.settings` / `services.openssh.enable` — same as NixOS (upstream openssh module)
- `programs.uwsm.enable` / `programs.uwsm.waylandCompositors` — same as NixOS module

### Custom namespaces

- `bubuntu.nix.envPath` — nix PATH in /etc/environment + sudoers (default: enabled)
- `bubuntu.apt.packages` — ensure apt packages are installed

### Generic modules (shared NixOS + system-manager)

- `modules.generic.public-keys` — provides `config.lib.publicKeys` helpers, works in both contexts
- `modules.generic.sshd-policy` — SSH policy (CA keys, principals, settings), imported by both `nixosModules.sshd` and `systemModules.sshd`

### Deployment

- deploy-rs with two profiles per node: `home` (home-manager, as user) and `system` (system-manager, sudo to root).
- `self.lib.deploy-system-manager` activation helper uses `deploy-rs.lib.activate.custom base "$PROFILE/bin/activate"`.
- Magic rollback via deploy-rs confirm-timeout provides safety for sshd changes.

## Module inventory

| Module file | systemModule name | What it does |
|---|---|---|
| `modules/bubuntu.nix` | `bubuntu` | Baseline: stubs, sysctl defaults, logind defaults |
| `modules/system-manager-sysctl.nix` | `sysctl` | `boot.kernel.sysctl` → /etc/sysctl.d + apply service |
| `modules/system-manager-nix-path.nix` | `nix-path` | Nix in PATH: /etc/environment, sudoers.d, environment.d |
| `modules/system-manager-logind.nix` | `logind` | `services.logind.settings` → logind.conf.d drop-in |
| `modules/system-manager-uwsm.nix` | `uwsm` | uwsm from nixpkgs, user units, profile.d path ordering, compositor desktop entries |
| `modules/system-manager-apt.nix` | `apt-packages` | `bubuntu.apt.packages` → apt-get install |
| `modules/sshd.nix` | `sshd` | Full sshd takeover with `managedUsers` option (NOT in baseline) |

## Key patterns

### Disabling upstream modules

Use the directory path (not `default.nix`) for `disabledModules`:
```nix
"${inputs.system-manager}/nix/modules/upstream/nixpkgs"
```

### Placing systemd user units

`environment.etc."systemd/user/<unit-name>"` — maps to `/etc/systemd/user/` which systemd reads for user units.

### Placing files outside /etc

Use `systemd.tmpfiles.rules` with `L+` for symlinks (e.g. `/usr/local/share/wayland-sessions/`).

### Programs needing system PAM

Keep them as .deb packages (e.g. swaylock). Use `bubuntu.apt.packages` to ensure they're installed. Nix-built binaries can't use system PAM modules (different library paths). Use a shell wrapper overlay in home-manager to delegate to `/usr/bin/<program>`.

### deploy-rs warnings suppression

- Set `services.openssh.openFirewall = false` (system-manager doesn't manage firewall)
- Set `systemd.services."ssh-system-manager".aliases = lib.mkForce []` (system-manager doesn't support unit aliases)

### The `--no-sandbox` situation (Slack/Electron)

With `kernel.unprivileged_userns_clone=1` and `kernel.apparmor_restrict_unprivileged_userns=0` (set by bubuntu baseline), Chromium's namespace sandbox works without `--no-sandbox`. The Slack wrapper was removed.
