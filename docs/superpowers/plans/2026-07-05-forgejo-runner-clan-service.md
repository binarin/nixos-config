# Forgejo-runner clan service — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the repo-scoped, registration-token CI runners with a `forgejo-runner` clan service that registers owner-scoped runners via Forgejo's modern shared-secret flow, so the runners serve every repo under the `binarin` user.

**Architecture:** A new clan service `flake.clan.modules.forgejo-runner` (modeled on `modules/clan/postgresql.nix`) with a `server` role (the forgejo host, which idempotently pre-registers each runner) and a `runner` role (each builder host, which runs `forgejo-runner daemon` directly). A `share = true` clan-vars generator per runner distributes one 40-hex secret to both sides; the runner UUID is derived from the secret, so no registration handshake is needed.

**Tech Stack:** Nix / flake-parts (dendritic, `import-tree ./modules`), clan-core clan services + clan vars, systemd, forgejo 15.0.3, forgejo-runner 12.12.0.

## Global Constraints

- **`import-tree ./modules`** auto-imports every `.nix` under `modules/` — new files need no explicit import. (`flake.nix`)
- **Secrets** use clan vars generators (`share = true`), never `/nix/store`. Reference deployed files via `config.clan.core.vars.generators.<gen>.files.<file>.path`.
- **forgejo-cli naming:** `forgejo actions register` only works when the executable **file** is named `forgejo-cli` (`os.Executable` basename check). Symlinks and `exec -a` do NOT work — copy the wrapped ELF: `cp ${forgejoPkg}/bin/.forgejo-wrapped $out/bin/forgejo-cli`.
- **Runner identity:** `token` = the full 40-hex secret; `uuid = uuid(secret[:16] bytes)`. Verified bash derivation: `hex=$(printf '%s' "${secret:0:16}" | od -An -tx1 | tr -d ' \n'); uuid="${hex:0:8}-${hex:8:4}-${hex:12:4}-${hex:16:4}-${hex:20:12}"`.
- **Naming (multi-instance safe):** every generated identifier is namespaced `ident = "${instanceName}-${machine}-${idx}"`; Forgejo display name is `machine` (idx 1) or `machine-${idx}`.
- **Persistent runners** (not ephemeral). Labels `native:host`. Scope `binarin`.
- **Forgejo DB is sqlite** → register oneshots must serialize (single writer) and use `Restart=on-failure`.
- **Eval/build commands:** `just eval-nixos <host>` (= `ncf eval nixos <host>`), `ncf build <host>`, `nix fmt`, `./scripts/check-module-keys.sh`. Formatting must pass before every commit.
- Commit messages end with the trailer `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.

**Spec:** `docs/superpowers/specs/2026-07-05-forgejo-runner-clan-service-design.md`

---

### Task 1: Create the `forgejo-runner` clan service module

**Files:**
- Create: `modules/clan/forgejo-runner.nix`

**Interfaces:**
- Produces: `flake.clan.modules.forgejo-runner` — a `_class = "clan.service"` module with:
  - role `runner` settings: `count` (positive int, default 1), `labels` (list str, default `["native:host"]`), `url` (str, default `"https://forgejo.lynx-lizard.ts.net"`), `hostPackages` (list package, default `[]`), `supplementaryGroups` (list str, default `["podman"]`).
  - role `server` settings: `scope` (str, default `"binarin"`).
  - Emits systemd units `forgejo-runner-${ident}.service` (runner hosts) and `forgejo-register-${ident}.service` (server host), and per-runner `share=true` generators named `forgejo-runner-${ident}`.

- [ ] **Step 1: Write the module file**

Create `modules/clan/forgejo-runner.nix` with exactly this content:

```nix
{ ... }:
{
  flake.clan.modules.forgejo-runner =
    { lib, ... }:
    let
      # --- identity helpers (instance-namespaced so multiple instances co-exist) ---
      identOf = instanceName: machine: idx: "${instanceName}-${machine}-${toString idx}";
      displayNameOf = machine: idx: if idx == 1 then machine else "${machine}-${toString idx}";
      genNameOf = instanceName: machine: idx: "forgejo-runner-${identOf instanceName machine idx}";
      registerUnitOf = ident: "forgejo-register-${ident}";
      daemonUnitOf = ident: "forgejo-runner-${ident}";

      # One runner-role machine's settings -> flat list of runner descriptors.
      runnersForMachine =
        instanceName: machine: settings:
        map (idx: {
          inherit idx machine instanceName;
          ident = identOf instanceName machine idx;
          displayName = displayNameOf machine idx;
          genName = genNameOf instanceName machine idx;
          inherit (settings) labels url;
        }) (lib.range 1 settings.count);

      # All runners across all runner-role machines of one instance (server view).
      runnersForInstance =
        instanceName: roles:
        lib.concatLists (
          lib.mapAttrsToList (
            machine: m: runnersForMachine instanceName machine m.settings
          ) (roles.runner.machines or { })
        );
    in
    {
      _class = "clan.service";
      manifest.name = "nixos-config-forgejo-runner";
      manifest.description = "Provision owner-scoped Forgejo Actions runners via the shared-secret (idempotent) registration flow";
      manifest.readme = ''
        A Forgejo host (`server` role) pre-registers a runner per (`runner`-role
        machine, index) using `forgejo-cli actions register --secret-file …
        --scope <owner>`, idempotent because the runner UUID is derived from the
        secret. Each builder host (`runner` role) runs `forgejo-runner daemon`
        directly with that UUID + shared secret — no registration-token exchange,
        no `.runner` file, nothing deprecated. The secret is one `share = true`
        clan-vars generator per runner (declared in `perMachine`), decrypted by
        both the forgejo host and the owning builder. Register oneshots are
        serialized across all instances because Forgejo's sqlite DB is single-writer.
      '';

      # ---------------- runner role: builder host ----------------
      roles.runner = {
        description = "A builder host running one or more forgejo-runner daemons, authenticating with a pre-registered shared secret.";
        interface =
          { lib, ... }:
          {
            options = {
              count = lib.mkOption {
                type = lib.types.ints.positive;
                default = 1;
                description = "Number of forgejo-runner daemon instances on this host.";
              };
              labels = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ "native:host" ];
                description = "Runner labels; jobs select runners via runs-on.";
              };
              url = lib.mkOption {
                type = lib.types.str;
                default = "https://forgejo.lynx-lizard.ts.net";
                description = "Forgejo instance URL the daemon connects to.";
              };
              hostPackages = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                default = [ ];
                description = "Extra packages placed on PATH for host-execution jobs (added to a base toolset).";
              };
              supplementaryGroups = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ "podman" ];
                description = "Supplementary groups for each daemon (e.g. podman, nix-access-tokens).";
              };
            };
          };

        perInstance =
          { instanceName, settings, ... }:
          {
            nixosModule =
              {
                config,
                lib,
                pkgs,
                ...
              }:
              let
                machine = config.networking.hostName;
                runners = runnersForMachine instanceName machine settings;

                baseTools = with pkgs; [
                  bash
                  coreutils
                  curl
                  util-linux
                  forgejo-cli
                  gawk
                  gitMinimal
                  git-crypt
                  gnused
                  jq
                  just
                  nix
                  nodejs
                  wget
                  podman
                  s3cmd
                ];

                mkDaemon =
                  r:
                  let
                    secretPath = config.clan.core.vars.generators.${r.genName}.files.secret.path;
                    labelArgs = lib.concatMapStringsSep " " (l: "--label ${lib.escapeShellArg l}") r.labels;
                    baseConfig = (pkgs.formats.yaml { }).generate "forgejo-runner-${r.ident}.yaml" {
                      log.level = "info";
                      runner = {
                        file = "/var/lib/gitea-runner/${r.ident}/.runner";
                        capacity = 1;
                      };
                      cache.enabled = true;
                    };
                    startScript = pkgs.writeShellScript "forgejo-runner-${r.ident}-start" ''
                      set -euo pipefail
                      secret=$(cat "$CREDENTIALS_DIRECTORY/token")
                      hex=$(printf '%s' "''${secret:0:16}" | od -An -tx1 | tr -d ' \n')
                      uuid="''${hex:0:8}-''${hex:8:4}-''${hex:12:4}-''${hex:16:4}-''${hex:20:12}"
                      exec ${lib.getExe pkgs.forgejo-runner} daemon \
                        --config ${baseConfig} \
                        --url ${lib.escapeShellArg r.url} \
                        --uuid "$uuid" \
                        --token-url "file:$CREDENTIALS_DIRECTORY/token" \
                        ${labelArgs}
                    '';
                  in
                  lib.nameValuePair (daemonUnitOf r.ident) {
                    description = "Forgejo Actions runner ${r.displayName}";
                    wants = [ "network-online.target" ];
                    after = [
                      "network-online.target"
                      "podman.socket"
                    ];
                    wantedBy = [ "multi-user.target" ];
                    environment.DOCKER_HOST = "unix:///run/podman/podman.sock";
                    path = [ pkgs.coreutils ] ++ baseTools ++ settings.hostPackages;
                    serviceConfig = {
                      Type = "simple";
                      DynamicUser = true;
                      User = "forgejo-runner";
                      StateDirectory = "gitea-runner/${r.ident}";
                      WorkingDirectory = "/var/lib/gitea-runner/${r.ident}";
                      SupplementaryGroups = settings.supplementaryGroups;
                      LoadCredential = [ "token:${secretPath}" ];
                      ExecStart = startScript;
                      Restart = "on-failure";
                      RestartSec = 5;
                    };
                  };
              in
              {
                virtualisation.podman.enable = true;
                systemd.services = lib.listToAttrs (map mkDaemon runners);
              };
          };
      };

      # ---------------- server role: forgejo host ----------------
      roles.server = {
        description = "The Forgejo host: idempotently registers every runner via the shared-secret flow.";
        interface =
          { lib, ... }:
          {
            options.scope = lib.mkOption {
              type = lib.types.str;
              default = "binarin";
              description = "Owner scope for registration ({owner} or {owner}/{repo}); empty = global.";
            };
          };

        perInstance =
          {
            instanceName,
            settings,
            roles,
            ...
          }:
          {
            nixosModule =
              {
                config,
                lib,
                pkgs,
                ...
              }:
              let
                fcfg = config.services.forgejo;
                # `actions register` only exists when the executable FILE is named
                # `forgejo-cli` (os.Executable basename). Copy the wrapped ELF.
                # NB: this is the forgejo SERVER binary in forgejo-cli mode — NOT the
                # unrelated `pkgs.forgejo-cli` (0.5.0) API client used in job toolsets.
                forgejoServerCli = pkgs.runCommandLocal "forgejo-server-cli-${fcfg.package.version}" { } ''
                  mkdir -p $out/bin
                  test -x ${fcfg.package}/bin/.forgejo-wrapped \
                    || { echo "expected ${fcfg.package}/bin/.forgejo-wrapped (nixpkgs wrapProgram layout changed)"; exit 1; }
                  cp ${fcfg.package}/bin/.forgejo-wrapped $out/bin/forgejo-cli
                  chmod +x $out/bin/forgejo-cli
                '';
                runners = runnersForInstance instanceName roles;

                mkRegister =
                  r:
                  let
                    secretPath = config.clan.core.vars.generators.${r.genName}.files.secret.path;
                    regScript = pkgs.writeShellScript "forgejo-register-${r.ident}" ''
                      set -euo pipefail
                      exec ${forgejoServerCli}/bin/forgejo-cli actions register \
                        --secret-file "$CREDENTIALS_DIRECTORY/token" \
                        --scope ${lib.escapeShellArg settings.scope} \
                        --name ${lib.escapeShellArg r.displayName} \
                        --labels ${lib.escapeShellArg (lib.concatStringsSep "," r.labels)}
                    '';
                  in
                  lib.nameValuePair (registerUnitOf r.ident) {
                    description = "Register Forgejo runner ${r.displayName} (scope ${settings.scope})";
                    wantedBy = [ "multi-user.target" ];
                    requires = [ "forgejo.service" ];
                    after = [ "forgejo.service" ];
                    path = with pkgs; [
                      coreutils
                      gitMinimal
                      openssh
                      gzip
                    ];
                    environment = {
                      FORGEJO_WORK_DIR = fcfg.stateDir;
                      FORGEJO_CUSTOM = fcfg.customDir;
                    };
                    serviceConfig = {
                      Type = "oneshot";
                      RemainAfterExit = true;
                      Restart = "on-failure";
                      RestartSec = 5;
                      User = fcfg.user;
                      Group = fcfg.group;
                      LoadCredential = [ "token:${secretPath}" ];
                      ExecStart = regScript;
                    };
                  };
              in
              {
                systemd.services = lib.listToAttrs (map mkRegister runners);
              };
          };
      };

      # ---------------- shared secrets + register serialization ----------------
      perMachine =
        { instances, machine, ... }:
        {
          nixosModule =
            { lib, pkgs, ... }:
            let
              isServer = builtins.elem "server" machine.roles;

              # Every runner across every instance (server view).
              allRunners = lib.concatLists (
                lib.mapAttrsToList (instanceName: inst: runnersForInstance instanceName inst.roles) instances
              );
              # This machine's own runners across every instance (runner view).
              myRunners = lib.concatLists (
                lib.mapAttrsToList (
                  instanceName: inst:
                  let
                    m = inst.roles.runner.machines.${machine.name} or null;
                  in
                  if m == null then [ ] else runnersForMachine instanceName machine.name m.settings
                ) instances
              );

              baseGen = {
                share = true;
                runtimeInputs = [ pkgs.openssl ];
                # tr -d strips the trailing newline so the 40-hex secret is exact.
                script = "openssl rand -hex 20 | tr -d '\\n' > $out/secret";
              };
              serverGen =
                r:
                lib.nameValuePair r.genName (
                  baseGen
                  // {
                    files.secret = {
                      secret = true;
                      deploy = true;
                      restartUnits = [ "${registerUnitOf r.ident}.service" ];
                    };
                  }
                );
              clientGen =
                r:
                lib.nameValuePair r.genName (
                  baseGen
                  // {
                    files.secret = {
                      secret = true;
                      deploy = true;
                      restartUnits = [ "${daemonUnitOf r.ident}.service" ];
                    };
                  }
                );

              # Serialize register oneshots (sqlite single-writer). Stable order by ident.
              orderedIdents = map (r: r.ident) (lib.sort (a: b: a.ident < b.ident) allRunners);
              serialAfter = lib.listToAttrs (
                lib.imap0 (
                  i: ident:
                  lib.nameValuePair (registerUnitOf ident) {
                    after = lib.optional (i > 0) "${registerUnitOf (lib.elemAt orderedIdents (i - 1))}.service";
                  }
                ) orderedIdents
              );
            in
            {
              clan.core.vars.generators = lib.listToAttrs (
                (lib.optionals isServer (map serverGen allRunners)) ++ (map clientGen myRunners)
              );
              systemd.services = lib.optionalAttrs isServer serialAfter;
            };
        };
    };
}
```

- [ ] **Step 2: Format and check module keys**

Run: `nix fmt modules/clan/forgejo-runner.nix && ./scripts/check-module-keys.sh`
Expected: no formatting diff remains; check-module-keys passes (clan service modules under `flake.clan.modules` are exempt from the `key` requirement, mirroring `modules/clan/postgresql.nix`).

- [ ] **Step 3: Verify the flake still evaluates (nothing consumes the service yet)**

Run: `just eval-nixos forgejo`
Expected: PASS — the forgejo config evaluates unchanged (the service is registered but no instance uses it).

- [ ] **Step 4: Verify the forgejo-cli copy really unlocks `register` (the key risk)**

Run:
```bash
p=$(nix build --no-link --print-out-paths '.#configured-pkgs.x86_64-linux.nixpkgs.forgejo')
d=$(mktemp -d); cp "$p/bin/.forgejo-wrapped" "$d/forgejo-cli"
"$d/forgejo-cli" actions --help 2>&1 | sed -n '/COMMANDS/,/OPTIONS/p'; rm -rf "$d"
```
Expected: the COMMANDS list includes `register  Idempotent registration of a runner using a shared secret` and `generate-secret` (not just `generate-runner-token`).

- [ ] **Step 5: Commit**

```bash
git add modules/clan/forgejo-runner.nix
git commit -m "feat(forgejo-runner): add clan service for shared-secret CI runners

Server role registers runners via forgejo-cli actions register; runner
role runs forgejo-runner daemon with a UUID derived from a shared
clan-vars secret. Not wired to any machine yet.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Wire the `forgejo-runner` instance and slim `modules/nix-builder.nix`

**Files:**
- Modify: `modules/machines/forgejo.nix` (add server-role instance wiring)
- Modify: `modules/machines/nix-builder-valak.nix` (runner role count=3; drop `runnerCount`)
- Modify: `modules/machines/nix-builder-raum.nix` (runner role count=2; drop `runnerCount`)
- Modify: `modules/nix-builder.nix` (remove `services.gitea-actions-runner`, `runnerCount` option, sops token; keep host concerns)

**Interfaces:**
- Consumes: `flake.clan.modules.forgejo-runner` (Task 1) via `clan.inventory.instances.forgejo-runner`.

- [ ] **Step 1: Observe the current (old) runner units before changing them**

Run: `just eval-nixos nix-builder-valak >/dev/null && echo OK`
Then confirm the old service still exists (baseline to be removed):
Run: `nix eval --json '.#nixosConfigurations.nix-builder-valak.config.systemd.services' --apply 's: builtins.filter (lib.hasPrefix "gitea-runner") (builtins.attrNames s)' 2>/dev/null || nix eval '.#nixosConfigurations.nix-builder-valak.config.systemd.services' --apply builtins.attrNames | tr ' ' '\n' | grep gitea-runner`
Expected: shows `gitea-runner-nixos-config*` units (the old ones we are replacing).

- [ ] **Step 2: Add the server-role instance wiring to `forgejo.nix`**

In `modules/machines/forgejo.nix`, immediately after the `clan.machines.forgejo = { … };` block (before `flake.nixosConfigurations.forgejo`), add:

```nix
  clan.inventory.instances.forgejo-runner = {
    module = {
      input = "self";
      name = "forgejo-runner";
    };
    roles.server.machines.forgejo.settings.scope = "binarin";
  };
```

- [ ] **Step 3: Wire valak as a runner and drop its old `runnerCount`**

In `modules/machines/nix-builder-valak.nix`:

Add, right after the `clan.machines.nix-builder-valak = { … };` block:

```nix
  clan.inventory.instances.forgejo-runner.roles.runner.machines.nix-builder-valak.settings = {
    count = 3;
    hostPackages = [ inputs.niks3.packages.x86_64-linux.niks3 ];
    supplementaryGroups = [
      "podman"
      "nix-access-tokens"
    ];
  };
```

Remove this line from the inner `config = { … }` block:

```nix
        nixos-config.nix-builder.runnerCount = 3;
```

- [ ] **Step 4: Wire raum as a runner and drop its old `runnerCount`**

In `modules/machines/nix-builder-raum.nix`:

Add, right after the `clan.machines.nix-builder-raum = { … };` block:

```nix
  clan.inventory.instances.forgejo-runner.roles.runner.machines.nix-builder-raum.settings = {
    count = 2;
    hostPackages = [ inputs.niks3.packages.x86_64-linux.niks3 ];
    supplementaryGroups = [
      "podman"
      "nix-access-tokens"
    ];
  };
```

Remove this line from the inner `config = { … }` block:

```nix
        nixos-config.nix-builder.runnerCount = 2;
```

- [ ] **Step 5: Slim `modules/nix-builder.nix` to host-only concerns**

Replace the entire contents of `modules/nix-builder.nix` with the following. Note the outer header keeps `inputs` (needed for `inputs.srvos`) and drops `inputs'` (only the removed niks3/gitea code used it):

```nix
{
  self,
  inputs,
  ...
}:
{
  flake.nixosModules.nix-builder =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.nix-builder";
      imports = [
        "${inputs.srvos}/nixos/roles/nix-remote-builder.nix"

        self.nixosModules.impure-nix-setup
      ];

      config = {
        nixos-config.nix.accessTokens = {
          "github.com" = "extra-access-tokens/github.com";
        };

        nixos-config.export-metrics.enable = true;

        nix.settings.system-features = [
          "big-parallel"
        ];

        nix.extraOptions = ''
          build-dir = /nix/build
        '';

        users.users.nix-remote-builder.openssh.authorizedPrincipals = lib.forEach [
          "nix-remote-builder"
          "binarin"
          "root"
        ] (k: ''restrict,command="nix-daemon --stdio" ${k}'');
        roles.nix-remote-builder.schedulerPublicKeys = [ ];
      };
    };
}
```

- [ ] **Step 6: Format**

Run: `nix fmt modules/machines/forgejo.nix modules/machines/nix-builder-valak.nix modules/machines/nix-builder-raum.nix modules/nix-builder.nix`
Expected: files reformatted with no residual diff on a second run.

- [ ] **Step 7: Evaluate all three affected machines**

Run: `just eval-nixos forgejo && just eval-nixos nix-builder-valak && just eval-nixos nix-builder-raum`
Expected: all three PASS (no eval errors, no infinite recursion, no missing-attr errors).

- [ ] **Step 8: Verify the new runner daemon units exist and the old ones are gone**

Run:
```bash
nix eval '.#nixosConfigurations.nix-builder-valak.config.systemd.services' --apply builtins.attrNames | tr ' ' '\n' | grep -E 'forgejo-runner|gitea-runner'
```
Expected: three units `"forgejo-runner-forgejo-runner-nix-builder-valak-1"`, `-2`, `-3`; NO `gitea-runner-*` units.

- [ ] **Step 9: Verify the register units exist on the forgejo host with serialization**

Run:
```bash
nix eval '.#nixosConfigurations.forgejo.config.systemd.services' --apply builtins.attrNames | tr ' ' '\n' | grep forgejo-register
nix eval --raw '.#nixosConfigurations.forgejo.config.systemd.services."forgejo-register-forgejo-runner-nix-builder-valak-2".after' --apply 'a: builtins.concatStringsSep "," a' 2>/dev/null || \
  nix eval '.#nixosConfigurations.forgejo.config.systemd.services."forgejo-register-forgejo-runner-nix-builder-valak-2".after'
```
Expected: five `forgejo-register-forgejo-runner-*` units (valak-1..3, raum-1..2); the `-valak-2` unit's `after` list contains both `forgejo.service` and the lexicographically previous register unit (`forgejo-register-forgejo-runner-nix-builder-valak-1.service`).

- [ ] **Step 10: Build one runner host and the server end-to-end**

Run: `ncf build nix-builder-valak && ncf build forgejo`
Expected: both build to completion (this compiles the daemon wrapper, the base yaml, and the `forgejo-cli` copy derivation).

- [ ] **Step 11: Commit**

```bash
git add modules/machines/forgejo.nix modules/machines/nix-builder-valak.nix modules/machines/nix-builder-raum.nix modules/nix-builder.nix
git commit -m "feat(forgejo-runner): wire binarin-scoped runner instance; drop old runners

forgejo=server(scope binarin); valak=3, raum=2 runners. Removes the
gitea-actions-runner + registration-token wiring from nix-builder.nix,
keeping only host/remote-builder concerns.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: Verify runner identity, multi-instance safety, retire the old secret, full build

**Files:**
- Modify (delete stanza): the old sops secret declaration for `nixos-config-runner-token`, if it lives in a secrets manifest that still references it.

**Interfaces:**
- Consumes: everything from Tasks 1–2.

- [ ] **Step 1: Prove the daemon wrapper's UUID matches forgejo-runner's derivation**

Run:
```bash
S=$(openssl rand -hex 20)
d=$(mktemp -d)
( cd "$d" && nix run 'nixpkgs#forgejo-runner' -- create-runner-file --secret "$S" --instance https://x >/dev/null 2>&1; grep -o '"uuid": "[^"]*"' .runner )
hex=$(printf '%s' "${S:0:16}" | od -An -tx1 | tr -d ' \n')
echo "wrapper uuid: ${hex:0:8}-${hex:8:4}-${hex:12:4}-${hex:16:4}-${hex:20:12}"
rm -rf "$d"
```
Expected: the two UUIDs are identical (the plan's derivation matches forgejo-runner). If they differ, STOP — the daemon wrapper in Task 1 is wrong.

- [ ] **Step 2: Confirm the register ExecStart uses forgejo-cli and --secret-file (no secret in argv/store)**

Run:
```bash
nix eval --raw '.#nixosConfigurations.forgejo.config.systemd.services."forgejo-register-forgejo-runner-nix-builder-raum-1".serviceConfig.ExecStart'
```
Then `cat` the printed store path.
Expected: the script calls `.../forgejo-cli actions register --secret-file "$CREDENTIALS_DIRECTORY/token" --scope binarin --name nix-builder-raum --labels native:host` — the secret is read from the credentials dir, never a literal.

- [ ] **Step 3: Confirm the shared generators exist on both sides**

Run:
```bash
nix eval '.#nixosConfigurations.forgejo.config.clan.core.vars.generators' --apply builtins.attrNames | tr ' ' '\n' | grep forgejo-runner-forgejo-runner
nix eval '.#nixosConfigurations.nix-builder-valak.config.clan.core.vars.generators' --apply builtins.attrNames | tr ' ' '\n' | grep forgejo-runner-forgejo-runner
```
Expected: the forgejo host lists all five generators (`…-valak-1..3`, `…-raum-1..2`); valak lists only its own three. Both sides share the same generator names (so `share=true` deploys one secret to both).

- [ ] **Step 4: Multi-instance sanity check (namespacing) — temporary probe**

Temporarily append a second instance to `modules/machines/forgejo.nix` (server) to prove no unit/name collisions, then evaluate:

Add (temporarily):
```nix
  clan.inventory.instances.forgejo-runner-probe = {
    module = {
      input = "self";
      name = "forgejo-runner";
    };
    roles.server.machines.forgejo.settings.scope = "probe";
    roles.runner.machines.nix-builder-valak.settings.count = 1;
  };
```

Run: `just eval-nixos forgejo && just eval-nixos nix-builder-valak`
Expected: BOTH PASS (idents `forgejo-runner-probe-nix-builder-valak-1` vs `forgejo-runner-nix-builder-valak-1` do not collide).

Then **remove** the probe block and re-run:
Run: `git checkout -- modules/machines/forgejo.nix` (discard the probe) — or delete the added block manually.
Confirm: `git diff --stat modules/machines/forgejo.nix` shows no changes.

- [ ] **Step 5: Retire the old registration-token secret**

Find any remaining reference to the old secret:
Run: `grep -rn "nixos-config-runner-token" --include="*.nix" . ; grep -rln "nixos-config-runner-token" secrets/ 2>/dev/null`
Expected after Task 2: no `.nix` references remain. If a sops manifest under `secrets/nix-builder-valak/` or `secrets/nix-builder-raum/` still holds the `nixos-config-runner-token` key, remove that key from the manifest (edit via the repo's sops workflow). If nothing references it, leave the encrypted entry for the human to prune during deploy (documented below).

- [ ] **Step 6: Full evaluation and formatting gate**

Run: `nix fmt && ./scripts/check-module-keys.sh && just eval-all`
Expected: formatting clean, module-keys pass, every configuration evaluates.

- [ ] **Step 7: Full build of the touched hosts**

Run: `ncf build forgejo && ncf build nix-builder-valak && ncf build nix-builder-raum`
Expected: all three build.

- [ ] **Step 8: Commit**

```bash
git add -A
git commit -m "chore(forgejo-runner): retire old registration-token secret; verify identity

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Post-implementation (human deploy steps — not automated)

These require a live instance and are performed by the operator after merge:

1. **Confirm the owner slug.** `--scope binarin` must equal the exact Forgejo username. Verify in the Forgejo UI before deploying.
2. **Deploy order:** deploy `forgejo` first (so the register oneshots create the runner rows), then `nix-builder-valak` / `nix-builder-raum`.
3. **Verify:** on the forgejo host, `systemctl status 'forgejo-register-*'` (all `active (exited)`); in the UI, five runners under `binarin` with label `native:host`, state online. On each builder, `systemctl status 'forgejo-runner-*'` active, and `journalctl -u forgejo-runner-* | grep -iE 'register|create-runner-file'` returns nothing (modern path only).
4. **End-to-end:** in a *second* repo under `binarin`, add a job `runs-on: native` and confirm it runs.
5. **Cleanup (manual, one-time):** delete the old `nixos-config`-scoped runner rows in the Forgejo UI/CLI, and prune the retired `nixos-config-runner-token` sops entry if it was left encrypted.
6. **Reducing counts later** leaves stale runner rows — remove them manually (`register` only upserts; no pruning).

## Self-Review notes

- Spec coverage: server role (Task 1), runner role (Task 1), shared-secret `perMachine` (Task 1), naming/multi-instance (Task 1 + Task 3 Step 4), machine wiring (Task 2), nix-builder slim-down (Task 2 Step 5), serialization (Task 1 perMachine + Task 2 Step 9), out-of-scope pruning/migration (Post-implementation) — all covered.
- Identifier consistency: `identOf`, `genNameOf`, `registerUnitOf`, `daemonUnitOf` are defined once in Task 1 and every later reference (verification greps, unit names) uses the same derived strings.
- The single hard external assumption — that `${forgejo}/bin/.forgejo-wrapped` exists — is guarded by a build-time `test -x` in the `forgejoServerCli` derivation and independently verified in Task 1 Step 4.
