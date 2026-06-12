# Murmur System-Manager: Remaining Work

## Nix Configuration (nix.custom.conf)

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

