# b-db-k system-manager bootstrap

Add system-manager support for booking CentOS (bentos) KVM hosts, starting with db.k.b.

## Context

- **Target host:** db.k.b — CentOS Stream 9, puppet-managed, LDAP user allebedev (uid 15008352)
- **Reference host:** adb.k.b — same OS, nix installed (determinate), home-manager deployed, no system-manager yet
- **Current state on db.k.b:** no nix, dotfiles present (will be disabled), sudo works passwordless
- **End state:** `deploy .#b-db-k -s` deploys both system-manager and home-manager profiles

## Components

### 1. `modules/bentos.nix` — CentOS/RHEL system module

Minimal system-manager baseline for puppet-managed CentOS hosts. Unlike `bubuntu`, does NOT manage users, ssh, sudo, or security wrappers.

Imports:
- `self.systemModules.sysctl`
- `self.systemModules.nix-path`
- Upstream `nix.nix` from system-manager (for `nix.settings.trusted-users`)
- NixOS `config/nix.nix` and `misc/meta.nix` (required by nix module)

Provides:
- `system-manager` in systemPackages
- `kernel.yama.ptrace_scope = 0` (default)

Does NOT import: userborn, openssh, sudo, security-wrappers, users-groups, apt, uwsm, chrome-policies, logind, firewall.

### 2. `modules/machines/b-dev-kvm.nix` — split from murmur.nix

Moves all booking KVM host config out of murmur.nix:

- `flake.deploy.nodes.b-adb-k` — user profile only (unchanged)
- `flake.deploy.nodes.b-db-k` — user profile + new system profile
- `flake.homeConfigurations.b-dev-kvm` — shared home-manager config for both hosts
- `flake.homeModules.b-dev-kvm-configuration` — the home module
- `flake.systemConfigs.b-db-k` — new system-manager config using bentos

System config for b-db-k:
```nix
{
  environment.etc."nix/nix.custom.conf".text = ''
    trusted-users = allebedev root 15008352
  '';
}
```

This writes to `/etc/nix/nix.custom.conf` which the determinate installer's `nix.conf` includes via `!include nix.custom.conf`. We use `environment.etc` directly rather than `nix.settings` because the installer owns the main `nix.conf` and we must not conflict with it.

The `nix-path` module handles `/etc/environment.d/50-nix.conf` and the `nix-env-path` service that patches `/etc/environment`.

Deploy node:
```nix
flake.deploy.nodes.b-db-k = {
  hostname = "db.k.b";
  sshUser = "allebedev";
  profiles.user = {
    path = self.lib.deploy-home-manager self.homeConfigurations.b-dev-kvm;
  };
  profiles.system = {
    user = "root";
    path = self.lib.deploy-system-manager self.systemConfigs.b-db-k;
  };
};
```

### 3. `murmur.nix` cleanup

Remove from murmur.nix:
- `flake.deploy.nodes.b-adb-k`
- `flake.deploy.nodes.b-db-k`
- `flake.homeConfigurations.b-dev-kvm`
- `flake.homeModules.standalone-home-manager-zsh`
- `flake.homeModules.b-dev-kvm-configuration`

Keep in murmur.nix:
- Everything murmur-specific (system config, deploy node, `murmur-home-allebedev` module)

Note: `standalone-home-manager-zsh` moves to the new file. Murmur's `murmur-home-allebedev` still imports it via `self.homeModules.*` which works across files.

### 4. `scripts/bootstrap-bentos.sh` — idempotent bootstrap

Usage: `./scripts/bootstrap-bentos.sh db.k.b`

The script runs locally, orchestrates the full bootstrap of a target host. Primary mode builds locally and copies; `--remote-build` flag forces building on the target.

Steps:

1. **SSH to target, detect nix** — checks `/nix/var/nix/profiles/default/bin/nix` existence
2. **Install nix if missing** (on target via SSH) — determinate installer with:
   - `--no-confirm`
   - `--nix-build-user-id-base 666000`
   - `--nix-build-group-id 666000`
   - `--extra-conf 'trusted-users = allebedev root 15008352'`
3. **Build system-manager closure** — locally by default (`nix build .#systemConfigs.b-db-k`), or on target with `--remote-build`
4. **Copy closure to target** — `nix copy --to ssh://allebedev@<host> ./result` (skipped if remote-build)
5. **Activate on target via SSH** — sources nix-daemon.sh to get nix in PATH, then:
   - `nix-env --profile /nix/var/nix/profiles/system-manager-profiles/system-manager -i <path>`
   - `<path>/bin/activate`

After first activation, the nix-path module ensures PATH is set up correctly, and subsequent deploys work via:
```bash
deploy .#b-db-k -s
```

### 5. Home-manager: profile.d nullification

Add to `b-dev-kvm-configuration` zshrc (initContent or envExtra): the slow profile.d script zeroing. This is already present in the current config via `programs.zsh.envExtra` — the `sudo tee $slow_scripts < /dev/null` block. No change needed here; it moves with the file split.

## What's NOT in scope

- sops/secrets (later)
- sshd or user management (puppet)
- sudoers management (puppet)
- dotfiles migration/removal (separate concern)
- adb.k.b system-manager (can be added later using same bentos module)
- db3.k.b (same pattern when ready)

## Risks and mitigations

- **First activation env quirk:** The bootstrap script explicitly sources nix-daemon.sh and uses absolute paths to nix binaries. After first activation, nix-path module patches `/etc/environment` so future SSH sessions have nix in PATH.
- **Puppet conflicts:** We only manage `/etc/nix/nix.custom.conf`, `/etc/environment.d/50-nix.conf`, and systemd units under system-manager control. No overlap with puppet-managed files.
- **LDAP uid in trusted-users:** The numeric uid 15008352 is required because nix resolves the user at daemon startup; LDAP user "allebedev" may not resolve in all contexts.
