---
name: clan-proxmox-ct
description: Use when creating a new NixOS Proxmox LXC container machine in a clan-based nix-config, when asked to add a new container/host/CT/VM to the clan inventory, or when converting/clanifying an existing non-clan nixosConfiguration (including migrating its explicit sops secrets to clan vars)
---

# Clan Proxmox CT

## Overview

Add a new bare-minimum Proxmox LXC container to a clan nix-config. Three deliverables: machine module, IP allocation, git tracking.

## When to Use

- Creating a new Proxmox CT machine
- Setting up a new clan host from scratch
- Asked to add a new container/host/CT to the config
- **Converting an existing (non-clan) `nixosConfiguration` to clan** — see [Converting an Existing Config](#converting-an-existing-config-to-clan) below

**Not for:** VMs (use `qemu-guest` + disko, not `lxc`), physical machines, non-clan configs.

**New machine vs. conversion:** the [Steps](#steps) below build a *fresh* CT (generate all vars). A *conversion* keeps a live machine's identity — it **imports** the existing host-id, SSH host keys, and secrets instead of generating them. Use the conversion section for any machine that already exists and is deployed.

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

## Converting an Existing Config to Clan

Clanify an already-deployed, non-clan `flake.nixosConfigurations.<name>`. **Core principle: preserve the running machine's identity** — import its host-id, SSH host keys, and secrets rather than generating new ones, or you'll break SSH `known_hosts`, sops decryption, and Tailscale enrolment on the live box.

Two phases: **(1) clanify the machine definition**, then **(2) migrate explicit sops secrets to clan vars** (only if the machine has any).

#### Who runs which `clan vars` command — read this first

| Command | Who | Why |
|---------|-----|-----|
| `clan vars set …` | **you (assistant)** | encrypt-only — writes a value encrypted to the machine's recipients; needs no decryption |
| `clan vars generate …` | **user only** | decrypts the generator's secret **dependencies** (CA key, imported host key, …); your sops identity in this environment is **not** a recipient and it fails with a sops decrypt error (RC 128/`config file not found`) |

**This holds even when the generated output is non-secret** (e.g. `openssh-cert`) — `generate` still decrypts secret *inputs*, so it is still the user's to run. Do not run any `clan vars generate` yourself; hand the user the exact command. (`clan vars set`, being encrypt-only, is always safe for you.)

### Phase 1 — Clanify the machine definition

#### 1.1 Rewire the definition

In `modules/machines/<name>.nix`, replace the manual `nixosSystem` block:

```nix
# BEFORE
flake.nixosConfigurations.<name> = inputs.nixpkgs.lib.nixosSystem {
  pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  specialArgs.inventoryHostName = "<name>";
  modules = [ self.nixosModules.<name>-configuration ];
};
```

with the three clan blocks (`pkgs`/`modules` migrate into `clan.machines.<name>`):

```nix
# AFTER
clan.inventory.machines.<name> = {
  deploy.targetHost = flakeConfig.inventory.ipAllocation.<name>.home.primary.address;
};
clan.machines.<name> = {
  imports = [ self.nixosModules.<name>-configuration ];
  nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
};
flake.nixosConfigurations.<name> = lib.mkForce (
  self.clan.nixosConfigurations.<name>.extendModules {
    specialArgs.inventoryHostName = "<name>";
  }
);
```

Leave `flake.deploy.nodes.<name>` and the entire `flake.nixosModules.<name>-configuration` module **verbatim** — conversion only swaps the definition wiring.

#### 1.2 Fix the arg header

The new blocks need `lib` and `flakeConfig`. Update the file header:

```nix
{ self, config, lib, ... }:      # add lib
let
  flakeConfig = config;          # add this binding
in
{
  ...
```

Symptom if `lib` missing: `nix eval` → `error: undefined variable 'lib'`.

**`inputs`:** the old `nixosSystem` call used it, but the `<name>-configuration` module often still references `inputs.*` (e.g. `inputs.arion`, `inputs.sops-nix` in its `imports`). **Keep `inputs` if anything else in the file uses it; drop it only if truly unused** — `grep -n 'inputs\.' modules/machines/<name>.nix` before deciding. A stale `inputs` triggers no error, but a missing-but-needed one does.

#### 1.3 Import identity (set — do NOT generate)

The machine already has a stable identity. **`clan vars set`** it from the recorded values instead of `clan vars generate`:

```bash
# host-id — from inventory/host-id.toml (line: <name> = "<hex8>")
echo '<hex8>' | clan vars set <name> hostId/hostId

# SSH host key — pull off the LIVE machine. It is the ground truth; do NOT
# source it from the repo's own secrets/<name>/ssh_host_* sops files even
# though they exist and decrypt — always go to the running box.
ssh root@<live-host> cat /etc/ssh/ssh_host_ed25519_key     | clan vars set <name> openssh/ssh.id_ed25519
ssh root@<live-host> cat /etc/ssh/ssh_host_ed25519_key.pub | clan vars set <name> openssh/ssh.id_ed25519.pub

# Tailscale — placeholder; the machine stays joined, no re-enrol needed
echo - | clan vars set <name> tailscale-auth/tailscale-auth
```

| Var | New CT (generate) | Convert (import) |
|-----|-------------------|------------------|
| `hostId/hostId` | make up a fresh hex8 | `clan vars set` from `inventory/host-id.toml` |
| `openssh/ssh.id_ed25519{,.pub}` | generated | `ssh root@<live-host> cat … \| clan vars set` |
| `tailscale-auth/tailscale-auth` | real auth key | `echo - \| clan vars set` (placeholder) |

Confirm the imported `openssh/ssh.id_ed25519.pub` value matches the live box before moving on.

**If the machine uses clan's `sshd` service** (has an `openssh-cert` generator — check `clan vars list <name>`): after importing the host key, its `openssh-cert/ssh.id_ed25519-cert.pub` shows `<not set>`, because `clan vars set` does **not** cascade to dependents. The cert must be (re)generated so it certifies the **imported** key — **user runs** (it decrypts the CA + host key deps):

```bash
clan vars generate <name> --generator openssh-cert --no-regenerate   # user runs
```

`--no-regenerate` leaves the already-set `openssh`/`openssh-ca` values untouched and only fills the missing cert. Verify it certifies the live key (non-secret — read it directly): the fingerprint from `ssh-keygen -l -f vars/per-machine/<name>/openssh-cert/ssh.id_ed25519-cert.pub/value` must equal the live host's key fingerprint.

**Non-ed25519 host keys:** clan's sshd manages **only an ed25519** host key. If the machine still needs an RSA/ECDSA host key (e.g. `services.openssh.settings.HostKeyAlgorithms = "+ssh-rsa"` for a legacy client), importing only `openssh` loses it — see [1.3b](#13b-preserve-a-non-ed25519-host-key-if-still-needed).

#### 1.3b Preserve a non-ed25519 host key (if still needed)

Clan's sshd serves only ed25519. If the machine's config keeps an RSA/ECDSA host key alive for a legacy client, add a dedicated generator and merge it into `services.openssh.hostKeys` (the option is a list, so an extra definition concatenates → sshd serves both):

```nix
clan.core.vars.generators.openssh-host-rsa = {
  files.ssh_host_rsa_key = { };            # secret (default)
  files.ssh_host_rsa_key.pub = { secret = false; };
  script = ''ssh-keygen -t rsa -N "" -f "$out/ssh_host_rsa_key"'';   # only runs on fresh machines
};
services.openssh.hostKeys = [
  { type = "rsa"; path = config.clan.core.vars.generators.openssh-host-rsa.files.ssh_host_rsa_key.path; }
];
```

Then import the live RSA key (so the client's `known_hosts` stays valid) — **user runs** the `ssh …` pull the same way as the ed25519 key. Check `nix eval …config.services.openssh.hostKeys` shows **both** types before proceeding. This gates the teardown in [2.5](#25-tear-down-the-sops-secrets): do **not** delete a `ssh_host_rsa_key`/`ssh_host_ecdsa_key` that has no clan-vars replacement.

#### 1.4 Generate the remaining vars — **user runs**

```bash
clan vars generate <name>
```

Fills any vars/secrets not imported above (passwords, `state-version`, etc.). Run it **after** the imports so they're preserved. Interactive and secret-touching — leave it to the user (see the [command table](#who-runs-which-clan-vars-command--read-this-first)).

#### 1.5 Validate & first deploy

```bash
nix eval  .#nixosConfigurations.<name>.config.system.build.toplevel
nix build .#nixosConfigurations.<name>.config.system.build.toplevel --no-link --print-out-paths
```

`--no-link --print-out-paths` avoids leaving a `result` gc-root in the repo while still confirming the store path. Then, **first deploy via clan** (provisions the machine's age key for sops):

```bash
clan machines update <name> --build-host localhost   # user runs; first deploy ONLY
```

After this, normal deploy-rs works: `deploy .#<name> -s`.

### Phase 2 — Migrate explicit sops secrets to clan vars

Do this only after Phase 1 is deployed and verified. Repeat per machine.

#### 2.1 Enumerate what needs migrating

```bash
nix eval --json .#nixosConfigurations.<name>.config.sops.secrets
```

**Drop every key starting `vars/`** — those are already clan-managed. The remaining plain keys (e.g. `grafana/admin-password`) are the explicit sops secrets to migrate.

#### 2.2 Declare a generator — migrate ownership/permissions

```nix
clan.core.vars.generators.<gen> = {
  prompts.<file>.description = "...";
  files.<file> = {
    secret = true;
    owner = config.users.users.<svc>.name;   # CARRY OVER owner/group/mode from the old sops.secrets
  };
  # ...one prompts/files pair per secret...
  script = ''
    cat $prompts/<file> > $out/<file>
  '';
};
```

Match every `owner`/`group`/`mode` the old `sops.secrets."<...>"` set — dropping them silently breaks the consuming service.

**Secrets consumed via `sops.templates` / env-files** (not the simple `$__file{...path}` case — e.g. arion `env_file`, systemd `EnvironmentFile`): split config from secrets rather than forcing a 1:1 var. Established repo idiom (see `pi-box.nix`'s `binarin-git-credentials`):

- **Non-secret config → plain `pkgs.writeText`** (eval-time interpolation of UID/GID etc. is fine there; it lands in the store, which is correct for non-secrets).
- **One raw-secret generator per secret** (imported 1:1 in 2.3), plus **one dependent generator** (`dependencies = [ … ]`) whose script composes the secret env-file fragment reading `$in/<dep>/<file>`. A secret shared across several env-files (e.g. a DB password) is sourced once via `dependencies`, avoiding drift.
- The consumer takes a **list**: `env_file = [ "${nonSecretWriteText}" secretFragment.path ]`. Coerce the `writeText` derivation to a string with `"${…}"` — `env_file` wants strings, not derivations.
- **Keep generator scripts hermetic:** only `openssl` / `cat $in/…` / `printf`, **no nix interpolation** — all eval-time values belong in the `writeText`.

After building, confirm no secret leaked into the non-secret store file (`nix eval …env_file --apply 'l: builtins.readFile (builtins.elemAt l 0)'`).

#### 2.3 Import values from the old sops file — **user runs**

```bash
sops decrypt ./secrets/<name>/secrets.yaml --extract '["<k>"]["<subk>"]' | clan vars set <name> <gen>/<file>
```

One per secret. Because the prompts are now satisfied, a later `clan vars generate` won't re-prompt for them.

#### 2.4 Cut over the consumers (atomic, in `.nix`, before touching the yaml)

1. Rewire every reference: `config.sops.secrets."<k>/<subk>".path` → `config.clan.core.vars.generators.<gen>.files.<file>.path`.
2. Delete the now-orphaned `sops.secrets."<k>/<subk>"` blocks.
3. Verify: `nix build .#nixosConfigurations.<name>.config.system.build.toplevel --no-link --print-out-paths`.

#### 2.5 Tear down the sops secrets

**User runs** the `sops unset` (needs their sops key); **you** verify emptiness and remove files.

```bash
# user: unset each migrated key, PLUS tailscale-auth unconditionally
sops unset ./secrets/<name>/secrets.yaml '["<k>"]["<subk>"]'
sops unset ./secrets/<name>/secrets.yaml '["tailscale-auth"]'
```

Then verify the file holds no data keys (sops metadata remaining is normal — `sops decrypt` prints `{}`):

```bash
sops decrypt ./secrets/<name>/secrets.yaml    # {} == empty
```

**If it still shows keys, STOP** — those are unmigrated secrets; loop back to 2.2 for each. When empty, remove the emptied `secrets.yaml` plus the old `ssh_host_*` files — but **only the ones now superseded by a clan var**. List everything first (`ls secrets/<name>/`) and check each host-key type:

- `ssh_host_ed25519_key{,.pub}` → superseded by the imported `openssh` var → delete.
- `ssh_host_rsa_key`/`ssh_host_ecdsa_key` (+`.pub`) → delete **only if** you gave it a replacement generator in [1.3b](#13b-preserve-a-non-ed25519-host-key-if-still-needed) **or** the machine genuinely no longer needs it. If a non-ed25519 key is still in use and has no clan-vars replacement, deleting it discards the only copy of that identity — go back to 1.3b.
- Keep `user-binarin.yaml` (and any other non-migrated user secret) **always**.

```bash
git rm -f secrets/<name>/secrets.yaml \
          secrets/<name>/ssh_host_ed25519_key secrets/<name>/ssh_host_ed25519_key.pub \
          secrets/<name>/ssh_host_rsa_key     secrets/<name>/ssh_host_rsa_key.pub
          # ...include ssh_host_ecdsa_key{,.pub} if present and superseded
```

Finally, trim the machine's rule in `.sops.yaml` — **narrow the `path_regex` only**:

```yaml
# BEFORE
- path_regex: secrets/<name>/(secrets.yaml|user-binarin.yaml|tailscale-auth)
# AFTER
- path_regex: secrets/<name>/user-binarin.yaml
```

Some machines instead have **separate** rule lines (`secrets/<name>/secrets.yaml`, `secrets/<name>/tailscale-auth`) rather than one combined alternation — delete those whole lines and keep only the `user-binarin.yaml` rule. Either way, `grep -n '<name>' .sops.yaml` afterwards should show only the `user-binarin.yaml` rule (plus the machine's anchor definition).

**Keep `*server_<name>` in `key_groups`** (the machine still decrypts `user-binarin.yaml`; its age identity survived because the SSH host key was imported). **Leave the shared `secrets/[^/]+/(ssh_host_…)` rule untouched** — it serves every machine.

**Ordering matters:** the `.sops.yaml` edit comes **after** `sops unset` and file deletion — sops needs the rule live to rewrite the file during `unset`.

#### 2.6 Redeploy (deploy-rs)

```bash
deploy .#<name> -s
```

Confirm the migrated service still works on the box (e.g. admin login) — that validates the sops→vars cutover.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Using `qemu-guest` instead of `lxc` | CTs use `lxc`, VMs use `qemu-guest` + disko |
| Setting MAC before host-id exists | Either set known MAC bytes first, or update MAC after `clan vars generate` |
| MAC/host-id mismatch | host-id = last 4 bytes of MAC, lowercase, no separators. Verify with step 5 |
| Forgetting `git add --intent-to-add` | Nix won't see the module file until git knows about it |
| Wrong file location | Machine modules go in `modules/machines/`, not `machines/` (those are hardware-config/disko) |
| Adding flake inputs for bare-minimum | Not needed — only add inputs if the machine uses external flakes (like niks3) |
| **Conversion:** generating instead of importing identity | Existing machines must `clan vars set` host-id/ssh/tailscale from live values — generating changes identity and breaks the running box |
| **Conversion:** forgetting `lib`/`flakeConfig` in the header | Rewired blocks need `lib` and `let flakeConfig = config;`; symptom is `undefined variable 'lib'` |
| **Conversion:** dropping owner/group/mode when declaring the generator | Carry over every permission the old `sops.secrets` set, or the service can't read its secret |
| **Conversion:** editing `.sops.yaml` before `sops unset` | `sops unset` needs the rule live; narrow `path_regex` only *after* keys are removed and files deleted |
| **Conversion:** removing `*server_<name>` or the shared `ssh_host` rule | Keep both — machine still decrypts `user-binarin.yaml`; the `ssh_host` rule is shared across all machines |
| **Conversion:** deleting `user-binarin.yaml` | Never — it stays; only `secrets.yaml` + superseded `ssh_host_*` files are removed |
| **Conversion:** running `clan vars generate` yourself | Always the user's — it decrypts secret deps your identity can't (RC 128), *even for non-secret outputs* like `openssh-cert`. Only `clan vars set` is yours |
| **Conversion:** dropping `inputs` from the header | Only if truly unused — the config module often still needs `inputs.arion`/`inputs.sops-nix`; `grep 'inputs\.'` first |
| **Conversion:** sourcing the ssh host key from local sops files | Pull from the live box (`ssh root@<live-host> cat …`) — that's ground truth; the repo copy may be stale |
| **Conversion:** forgetting `openssh-cert` after importing the host key | On clan-sshd machines the cert is `<not set>` (set doesn't cascade); user runs `clan vars generate <name> --generator openssh-cert --no-regenerate`, then verify the fingerprint matches the live key |
| **Conversion:** deleting an RSA/ECDSA host key clan doesn't manage | Clan sshd serves only ed25519; if a legacy client needs `+ssh-rsa`, add a host-key generator + `services.openssh.hostKeys` merge and import the live key *before* removing the sops file |
| **Conversion:** nix interpolation inside a generator `script` | Keep scripts hermetic (`openssl`/`cat`/`printf` only); put eval-time values (UID/GID) in a `writeText`, and coerce it to a string (`"${…}"`) for `env_file` |

## Machine File Location

- **Machine modules:** `modules/machines/<name>.nix` — clan machine definition
- **Hardware configs:** `machines/<name>/hardware-configuration.nix` — not needed for CTs
- **Disko:** `machines/<name>/disko.nix` — not needed for CTs
