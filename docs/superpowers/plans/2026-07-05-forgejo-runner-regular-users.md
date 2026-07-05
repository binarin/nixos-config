# forgejo-runner Regular Users Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace `DynamicUser` in the forgejo-runner clan service with a real per-instance user so systemd sets `HOME`/`USER`/`SHELL` from passwd, deleting the manual `HOME`/`XDG_*` env overrides.

**Architecture:** Edit only the `runner` role's `perInstance.nixosModule` in `modules/clan/forgejo-runner.nix`. Declare one `users.users.${instanceName}` + group + tmpfiles home once (outside the daemon loop); switch each daemon's `serviceConfig` from `DynamicUser` to `User`/`Group = instanceName` with per-daemon `StateDirectory`/`WorkingDirectory` under the shared home; strip the three env hacks. The `server` role and the `perMachine` secret/serialization logic are untouched.

**Tech Stack:** Nix, NixOS modules, clan services, systemd.

## Global Constraints

- Change scope is `modules/clan/forgejo-runner.nix`, **runner role only**. Do not touch the `server` role, the `perMachine` secrets/serialization block, or any machine module.
- User identity is keyed on `instanceName` (name = group = `instanceName`; home = `/var/lib/${instanceName}`). Currently the only instance is `forgejo-runner`.
- `isSystemUser = true`, **no fixed uid** (auto system UID).
- `shell = pkgs.bashInteractive`.
- Per-daemon subdir disambiguator is `sub = r.displayName` — for `idx == 1` this is the bare machine name (e.g. `nix-builder-valak`), otherwise `${machine}-${idx}`.
- Keep only `DOCKER_HOST` in `environment`. Delete `HOME`, `XDG_CACHE_HOME`, `XDG_CONFIG_HOME`.
- Do NOT re-add DynamicUser hardening (PrivateTmp etc.). Do NOT auto-delete old `/var/lib/gitea-runner/*` state.
- Existing `SupplementaryGroups`, `LoadCredential`, `ExecStart`, `Restart`/`RestartSec`, ordering, and `path` stay as-is.

---

### Task 1: Switch runner daemons from DynamicUser to a per-instance user

**Files:**
- Modify: `modules/clan/forgejo-runner.nix` — the `roles.runner.perInstance.nixosModule` `let`-body (`mkDaemon`, `baseConfig`, `environment`, `serviceConfig`) and its returned attrset (currently `modules/clan/forgejo-runner.nix:117-180`).
- Test: eval of `nix-builder-valak` + `nix-builder-raum` nixosConfigurations (no unit test framework — NixOS eval is the test).

**Interfaces:**
- Consumes (already in scope in this module): `instanceName` (from `perInstance` args), `r.ident`, `r.displayName`, `r.url`, `r.labels`, `settings.supplementaryGroups`, `settings.hostPackages`, `config.clan.core.vars.generators.${r.genName}.files.secret.path`, `pkgs`, `lib`.
- Produces: no cross-task interface (single-task plan). On-disk contract: each daemon `forgejo-runner-${r.ident}.service` runs as `User=${instanceName}` with `HOME=/var/lib/${instanceName}`, `WorkingDirectory=/var/lib/${instanceName}/${r.displayName}`.

- [ ] **Step 1: Capture the current eval as the baseline (must succeed before editing)**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config
nix eval --raw '.#nixosConfigurations.nix-builder-valak.config.systemd.services.forgejo-runner-forgejo-runner-nix-builder-valak-1.serviceConfig.User'
```
Expected: prints `forgejo-runner` (the current fixed DynamicUser name). This confirms the attr path used in Step 6.

- [ ] **Step 2: Add the user/group/home declaration to the returned module**

In `roles.runner.perInstance.nixosModule`, the module currently returns (at `modules/clan/forgejo-runner.nix:176-180`):

```nix
              in
              {
                virtualisation.podman.enable = true;
                systemd.services = lib.listToAttrs (map mkDaemon runners);
              };
```

Replace that returned attrset with:

```nix
              in
              {
                virtualisation.podman.enable = true;

                users.groups.${instanceName} = { };
                users.users.${instanceName} = {
                  isSystemUser = true;
                  group = instanceName;
                  home = "/var/lib/${instanceName}";
                  createHome = true;
                  shell = pkgs.bashInteractive;
                  description = "Forgejo Actions runner (${instanceName})";
                };

                systemd.tmpfiles.rules = [
                  "d /var/lib/${instanceName} 0750 ${instanceName} ${instanceName} -"
                ];

                systemd.services = lib.listToAttrs (map mkDaemon runners);
              };
```

- [ ] **Step 3: Point the runner config file at the shared-home subdir**

In `mkDaemon`, the `baseConfig` currently reads (at `modules/clan/forgejo-runner.nix:122-129`):

```nix
                    baseConfig = (pkgs.formats.yaml { }).generate "forgejo-runner-${r.ident}.yaml" {
                      log.level = "info";
                      runner = {
                        file = "/var/lib/gitea-runner/${r.ident}/.runner";
                        capacity = 1;
                      };
                      cache.enabled = true;
                    };
```

Change the `file` line to:

```nix
                        file = "/var/lib/${instanceName}/${r.displayName}/.runner";
```

- [ ] **Step 4: Strip the env hacks — keep only DOCKER_HOST**

The daemon's `environment` currently reads (at `modules/clan/forgejo-runner.nix:151-161`):

```nix
                    environment = {
                      DOCKER_HOST = "unix:///run/podman/podman.sock";
                      # Jobs inherit the daemon's env; without HOME the dynamic user
                      # gets HOME=/ and tools (nix, git) try to write to /.cache on
                      # the read-only root. Point HOME (and the XDG bases) at the
                      # writable state dir. NB: this does not reach tools run inside
                      # Bazel action sandboxes — those need --action_env in the repo.
                      HOME = "/var/lib/gitea-runner/${r.ident}";
                      XDG_CACHE_HOME = "/var/lib/gitea-runner/${r.ident}/.cache";
                      XDG_CONFIG_HOME = "/var/lib/gitea-runner/${r.ident}/.config";
                    };
```

Replace with:

```nix
                    environment = {
                      # HOME/USER/SHELL now come from the real user's passwd entry
                      # (see users.users.${instanceName}); unset XDG bases default to
                      # $HOME/.cache and $HOME/.config. NB: this does not reach tools
                      # run inside Bazel action sandboxes — those need --action_env.
                      DOCKER_HOST = "unix:///run/podman/podman.sock";
                    };
```

- [ ] **Step 5: Swap DynamicUser for the real user in serviceConfig**

The daemon's `serviceConfig` currently reads (at `modules/clan/forgejo-runner.nix:163-174`):

```nix
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
```

Replace with:

```nix
                    serviceConfig = {
                      Type = "simple";
                      User = instanceName;
                      Group = instanceName;
                      StateDirectory = "${instanceName}/${r.displayName}";
                      WorkingDirectory = "/var/lib/${instanceName}/${r.displayName}";
                      SupplementaryGroups = settings.supplementaryGroups;
                      LoadCredential = [ "token:${secretPath}" ];
                      ExecStart = startScript;
                      Restart = "on-failure";
                      RestartSec = 5;
                    };
```

- [ ] **Step 6: Eval both runner hosts — verify the new user, home, and no XDG overrides**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config
for h in nix-builder-valak nix-builder-raum; do
  echo "== $h =="
  nix eval --json ".#nixosConfigurations.$h.config.systemd.services.forgejo-runner-forgejo-runner-$h-1.serviceConfig" \
    | jq '{User, Group, StateDirectory, WorkingDirectory, DynamicUser}'
  nix eval --json ".#nixosConfigurations.$h.config.systemd.services.forgejo-runner-forgejo-runner-$h-1.environment" | jq
  nix eval --raw ".#nixosConfigurations.$h.config.users.users.forgejo-runner.home"
  echo
done
```
Expected, per host:
- `User` and `Group` = `"forgejo-runner"`; `DynamicUser` = `null`; `WorkingDirectory` = `"/var/lib/forgejo-runner/nix-builder-<host-suffix>"`; `StateDirectory` = `"forgejo-runner/nix-builder-<host-suffix>"`.
- `environment` = `{"DOCKER_HOST":"unix:///run/podman/podman.sock"}` only — no `HOME`, no `XDG_*`.
- `users.users.forgejo-runner.home` = `/var/lib/forgejo-runner`.

Note: `-1` daemon's `displayName` is the bare machine name, so its subdir is `nix-builder-valak` / `nix-builder-raum` (no `-1`). Spot-check a `-2` daemon to confirm the numbered form:
```bash
nix eval --json '.#nixosConfigurations.nix-builder-valak.config.systemd.services.forgejo-runner-forgejo-runner-nix-builder-valak-2.serviceConfig.WorkingDirectory'
```
Expected: `"/var/lib/forgejo-runner/nix-builder-valak-2"`.

- [ ] **Step 7: Full flake check**

Run:
```bash
cd /home/binarin/personal-workspace/nixos-config
nix flake check
```
Expected: completes without error (the whole flake evaluates; no assertion failures from the user/group change).

- [ ] **Step 8: Commit**

```bash
cd /home/binarin/personal-workspace/nixos-config
git add modules/clan/forgejo-runner.nix
git commit -m "feat(forgejo-runner): run daemons as a real per-instance user

Replace DynamicUser with a real users.users.\${instanceName} (isSystemUser,
auto uid, home /var/lib/forgejo-runner, bashInteractive shell). systemd now
sets HOME/USER/SHELL from passwd, so the manual HOME/XDG_CACHE_HOME/
XDG_CONFIG_HOME env overrides are gone. Per-daemon StateDirectory/
WorkingDirectory move under the shared home (subdir = displayName). The
disposable .runner file is regenerated at its new path (registration is
idempotent via secret-derived UUID); old /var/lib/gitea-runner/* is left as
harmless orphaned state.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Post-implementation (manual, on a runner host after deploy)

Not part of the automated task — verification against a live host per the spec:

- `systemctl show -p User -p Group -p Environment forgejo-runner-forgejo-runner-nix-builder-valak-1.service` → `User=forgejo-runner`, `HOME=/var/lib/forgejo-runner`, no `XDG_*`.
- `getent passwd forgejo-runner` → shows home `/var/lib/forgejo-runner` and shell.
- Run one real CI job; confirm nix/git write under `/var/lib/forgejo-runner/.cache` and the job passes.
- Optional one-off cleanup: `rm -rf /var/lib/gitea-runner` on each runner host once the new daemons are confirmed healthy.
