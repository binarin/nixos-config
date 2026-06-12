# Murmur System-Manager: Remaining Configuration Plan

## Current State

system-manager is deployed on murmur with:
- `systemModules.sysctl` — `boot.kernel.sysctl` (NixOS-compatible interface)
- `systemModules.nix-path` — `/etc/environment`, sudoers, environment.d for nix in PATH
- `systemModules.bubuntu` — baseline that composes the above, replaces upstream stubs

Manually managed items remaining on murmur that should be codified:

---

## 1. Niri/UWSM Session Module

**Goal:** Replace hand-managed `/usr/local/bin/uwsm-niri`, `/usr/local/share/wayland-sessions/niri-uwsm.desktop`, and the `zz-prefer-nix-paths.sh` profile.d script with a system-manager module.

**Current state on murmur:**
- uwsm installed from a locally-built .deb (`0.26.4-1~local0`)
- `/usr/local/bin/uwsm-niri` — shell wrapper that sources nix env, sets XDG_DATA_DIRS, then `exec nixGLIntel uwsm start -N niri -D niri -C niri -e -- "$(which niri-session)"`
- `/usr/local/share/wayland-sessions/niri-uwsm.desktop` — display manager session entry pointing to the wrapper
- `/etc/profile.d/zz-prefer-nix-paths.sh` — reorders PATH/XDG_DATA_DIRS/XDG_CONFIG_DIRS to prefer nix paths over system paths (uses perl one-liners)
- No apparmor profiles for uwsm found

**NixOS uwsm module reference** (`nixos/modules/programs/wayland/uwsm.nix`):
- Installs uwsm package + systemd units
- Creates wayland-sessions desktop entries per compositor
- Links `/share/uwsm` and `/share/wayland-sessions`
- Recommends dbus-broker

**Approach:**
- Create `systemModules.uwsm` with similar options to NixOS module
- Install uwsm from nixpkgs (replacing the .deb) — needs investigation if uwsm's systemd user units can be installed via system-manager's systemd user support or via environment.etc
- Generate `/usr/local/share/wayland-sessions/niri-uwsm.desktop` (or `/etc` equivalent)
- Replace `zz-prefer-nix-paths.sh` with a proper profile.d script that prepends nix paths to XDG_DATA_DIRS/XDG_CONFIG_DIRS
- The nixGLIntel wrapping is still needed (non-NixOS GPU drivers)

**Open questions:**
- Can we `apt purge uwsm` and rely entirely on nix-installed uwsm? The .deb installs systemd user units into `/usr/lib/systemd/user/` which is the standard path. system-manager may not manage user units there.
- Does the display manager (gdm/sddm?) look at `/usr/local/share/wayland-sessions/` or only `/usr/share/wayland-sessions/`?

---

## 2. logind.conf Module

**Goal:** Manage systemd-logind settings declaratively.

**Current manual config:**
```ini
[Login]
HandleLidSwitchExternalPower=ignore
HandleLidSwitchDocked=ignore
```

**Approach:**
- Create `systemModules.logind` with `services.logind.settings` option (attrsOf attrsOf — INI sections)
- Use `environment.etc."systemd/logind.conf.d/50-system-manager.conf"` drop-in (preferred over replacing the main file)
- Match NixOS option names where possible: `services.logind.lidSwitch`, `services.logind.lidSwitchExternalPower`, `services.logind.lidSwitchDocked`
- Service to `systemctl restart systemd-logind` on change (or rely on daemon-reload)

**Complexity:** Low — straightforward INI file generation.

---

## 3. Full SSHD Setup

**Goal:** Manage sshd_config with trusted user CA keys, host certificates, etc.

**Current state:**
- `/etc/ssh/sshd_config` has minor tweaks (KbdInteractiveAuthentication=no, X11Forwarding=yes)
- No drop-in configs in `sshd_config.d/`
- Standard Ubuntu sshd

**Approach:**
- Create `systemModules.sshd` that generates `/etc/ssh/sshd_config.d/50-system-manager.conf`
- Options: `services.openssh.settings` (matching NixOS `services.openssh.settings` attrset)
- For CA keys: `services.openssh.authorizedKeysFiles`, `TrustedUserCAKeys` pointing to a managed file
- Restart sshd service after config changes

**Open questions:**
- Do we want to fully own sshd_config or just use drop-ins? Drop-ins are safer (don't break SSH access if config is wrong), but `Include` is at the top of the main config so drop-ins override.
- CA key distribution: static file via `environment.etc` or sops-managed secret?

---

## 4. Nix Configuration (nix.custom.conf)

**Goal:** Manage `/etc/nix/nix.custom.conf` declaratively.

**Current content:**
```
extra-access-tokens = github.com=github_pat_...
trusted-users = root allebedev
```

**Approach:**
- Create `systemModules.nix-daemon-config` with `nix.settings` (attrsOf) and `nix.secretSettings` options
- Non-secret settings via `environment.etc."nix/nix.conf.d/50-system-manager.conf"` (or the custom conf path if nix respects it)
- Secret settings (access tokens) via sops-nix integration

**Open questions:**
- system-manager already has `upstream/sops-nix.nix` — how mature is sops integration?
- Does determinate nix read from `/etc/nix/nix.custom.conf` specifically (nix-installer convention) or from standard nix.conf / conf.d/?
- Age key / sops key management on murmur — is there a key already available to root?

**Investigation needed:**
```
ssh murmur "sudo ls /etc/nix/; sudo cat /etc/nix/nix.conf"
ssh murmur "sudo ls /var/lib/sops* /root/.config/sops* 2>/dev/null"
```

---

## Priority Order

1. **logind.conf** — smallest scope, quick win
2. **sshd** — moderate scope, high value (infrastructure hardening)
3. **niri/uwsm** — moderate scope, needs investigation on user units
4. **nix.custom.conf** — blocked on sops integration investigation
