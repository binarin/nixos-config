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

**Goal:** Reuse the existing SSH policy (CA keys, principals, authorized keys) on murmur via system-manager, with the same generic module that NixOS machines use.

**Decision:** Full takeover on murmur (system-manager replaces sshd_config and manages the service). NOT part of the bubuntu baseline — explicit opt-in per machine.

**Current architecture in this repo:**
- `modules/public-keys.nix` → `flake.modules.generic.public-keys` — generic module providing `config.lib.publicKeys` helpers (works in any module system context)
- `modules/sshd.nix` → `flake.nixosModules.sshd` — NixOS-specific: enables openssh, sets TrustedUserCaKeys from public-keys, configures root authorized keys/principals
- system-manager already ships a full `services.openssh` module (same option interface as NixOS) at `upstream/nixpkgs/openssh.nix` — already imported by bubuntu

**Plan:**

1. **Extract generic SSH policy** from `modules/sshd.nix` into `flake.modules.generic.sshd-policy`:
   ```nix
   # Works in both NixOS and system-manager contexts
   {
     services.openssh.settings.PermitRootLogin = "prohibit-password";
     services.openssh.settings.TrustedUserCaKeys = "/etc/ssh/trusted_user_ca_keys";
     services.openssh.settings.AuthorizedPrincipalsFile = "/etc/ssh/authorized_principals.d/%u";
     environment.etc."ssh/trusted_user_ca_keys".text = /* from public-keys */;
   }
   ```

2. **Keep `nixosModules.sshd`** as a thin wrapper that imports the generic policy + adds NixOS-specific bits (firewall, `users.users.root.openssh`, `authorizedKeysInHomedir`).

3. **Create `systemModules.sshd`** (flake-parts wrapper) that:
   - Imports `modules.generic.sshd-policy`
   - Sets `services.openssh.enable = true`
   - Adds system-manager-specific config (authorized keys for `allebedev` user via `users.users`)

4. **In murmur config:** import `self.systemModules.sshd` — NOT imported by bubuntu baseline.

**Dependencies:**
- `modules.generic.public-keys` must work in system-manager context (it already uses only `lib` and `config.lib` — should be fine)
- system-manager's openssh module needs `programs.ssh` (already provided by `upstream/nixpkgs/programs/ssh.nix` which bubuntu imports)

**Safety:**
- deploy-rs confirm-timeout acts as rollback safety net
- system-manager openssh module includes `sshd -t` validation check before activation
- Test: deploy system profile first, verify SSH still works, then confirm

**Files to create/modify:**
- `modules/sshd.nix` — refactor: extract generic part, keep NixOS wrapper
- New: generic sshd-policy module (could be inline in sshd.nix or separate file)
- `modules/machines/murmur.nix` — add `self.systemModules.sshd` to systemConfigs modules list

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
