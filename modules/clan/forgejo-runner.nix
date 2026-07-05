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
                type = lib.types.listOf lib.types.raw;
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
                    environment = {
                      DOCKER_HOST = "unix:///run/podman/podman.sock";
                      # Jobs inherit the daemon's env; without HOME the dynamic user
                      # gets HOME=/ and tools (nix, git) try to write to /.cache on
                      # the read-only root. Point it at the writable state dir.
                      HOME = "/var/lib/gitea-runner/${r.ident}";
                    };
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
