# Vendored from upstream's NixOS module:
#   https://github.com/getpaseo/paseo/blob/v0.11.4/nix/module.nix
#
# Adaptations for this repo:
#   - wrapped in a flake-parts module exporting flake.nixosModules.paseo
#     (repo convention), with a module key.
#   - dropped the `services.paseo.allowedHosts` rename shim (no legacy users
#     here).
#   - the access password is mandatory. Clan machines use the
#     `paseo-auth` clan var; plain machines fall back to the legacy sops
#     setup (`paseo-auth.password` in the machine's default sops file).
{
  flake.nixosModules.paseo =
    {
      config,
      lib,
      options,
      pkgs,
      specialArgs,
      ...
    }:
    let
      cfg = config.services.paseo;
      isClan = specialArgs ? clan-core;
      hasSops = options ? sops;
      machineName =
        if isClan then config.clan.core.settings.machine.name else config.networking.hostName;
      clanEnvironmentVar = "vars/per-machine/${machineName}/paseo-auth/env";
    in
    {
      key = "nixos-config.modules.nixos.paseo";

      options.services.paseo = {
        enable = lib.mkEnableOption "Paseo, a self-hosted daemon for AI coding agents";

        package = lib.mkPackageOption pkgs "paseo" { };

        user = lib.mkOption {
          type = lib.types.str;
          default = "paseo";
          description = "User account under which Paseo runs.";
        };

        group = lib.mkOption {
          type = lib.types.str;
          default = "paseo";
          description = "Group under which Paseo runs.";
        };

        dataDir = lib.mkOption {
          type = lib.types.str;
          default =
            if cfg.user == "paseo" then "/var/lib/paseo" else "/home/${cfg.user}/.paseo";
          defaultText = lib.literalExpression ''
            if cfg.user == "paseo"
            then "/var/lib/paseo"
            else "/home/''${cfg.user}/.paseo"
          '';
          description = "Directory for Paseo state (PASEO_HOME). Stores agent data, config, and logs.";
        };

        port = lib.mkOption {
          type = lib.types.port;
          default = 6767;
          description = "Port for the Paseo daemon to listen on.";
        };

        listenAddress = lib.mkOption {
          type = lib.types.str;
          default = "127.0.0.1";
          description = "Address for the Paseo daemon to bind to.";
        };

        openFirewall = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether to open the firewall for the Paseo daemon port.";
        };

        hostnames = lib.mkOption {
          type = lib.types.either (lib.types.enum [ true ]) (lib.types.listOf lib.types.str);
          default = [ ];
          example = [
            ".example.com"
            "myhost.local"
          ];
          description = ''
            Hostnames the Paseo daemon accepts in the Host header (DNS rebinding protection).
            Localhost and IP addresses are always allowed by default.

            Use a leading dot to match a domain and all its subdomains
            (e.g. `".example.com"` matches `example.com` and `foo.example.com`).

            Set to `true` to allow any host (not recommended).
          '';
        };

        relay = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = ''
              Whether to enable relay-based remote access. When false, the daemon
              runs with `--no-relay` and only accepts direct (LAN/loopback)
              connections.
            '';
          };

          mode = lib.mkOption {
            type = lib.types.enum [
              "hosted"
              "remote"
            ];
            default = "hosted";
            description = ''
              How the daemon reaches the relay when `relay.enable = true`:

              - `"hosted"` (default): use the upstream `app.paseo.sh` relay.
                Preserves the current behavior; no extra options needed.
              - `"remote"`: connect to a self-hosted relay at
                `relay.host:relay.port`. Sets `PASEO_RELAY_ENDPOINT` and
                `PASEO_RELAY_USE_TLS` for the daemon.

              A `"local"` mode (running a relay on the same host as a systemd
              unit) is not yet implemented — the relay package currently only
              ships a Cloudflare Workers adapter. Tracked separately.
            '';
          };

          host = lib.mkOption {
            type = lib.types.str;
            default = "";
            example = "relay.example.com";
            description = ''
              Relay hostname. Required when `relay.mode = "remote"`.'';
          };

          port = lib.mkOption {
            type = lib.types.port;
            default = 443;
            description = "Relay port. Used when `relay.mode = \"remote\"`.";
          };

          useTls = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = ''
              Whether to use TLS when connecting to the relay. Used when `relay.mode = "remote"`.'';
          };

          publicUseTls = lib.mkOption {
            type = lib.types.nullOr lib.types.bool;
            default = null;
            description = ''
              Whether the public (client-facing) relay endpoint uses TLS.
              When `null` (default), the daemon falls back to `relay.useTls`.
              Override when the internal path is plain `ws://` behind a
              TLS-terminating reverse proxy.
            '';
          };
        };

        inheritUserEnvironment = lib.mkOption {
          type = lib.types.bool;
          default = cfg.user != "paseo";
          defaultText = lib.literalExpression ''cfg.user != "paseo"'';
          description = ''
            Whether to include the user's profile PATH in the service environment.

            When Paseo runs as a real user (not the default system user), AI agents
            need access to the user's tools (git, ssh, etc.). This adds the user's
            NixOS profile, home-manager profile (`~/.nix-profile/bin` and
            `~/.local/state/nix/profile/bin`), and system paths so agents can use
            them without manually setting PATH.

            Enabled by default when `user` is set to a non-default value.
          '';
        };

        environment = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
          example = lib.literalExpression ''
            {
              PASEO_RELAY_ENDPOINT = "relay.paseo.sh:443";
            }
          '';
          description = "Extra environment variables for the Paseo daemon.";
        };

        settings = lib.mkOption {
          type = (pkgs.formats.json { }).type;
          default = { };
          example = lib.literalExpression ''
            {
              daemon.mcp = { enabled = true; injectIntoAgents = false; };
              agents.providers.myAcp = {
                extends = "acp";
                label = "My Agent";
                command = { path = "/run/current-system/sw/bin/my-acp"; };
              };
              log.file = { level = "info"; path = "/var/lib/paseo/daemon.log"; };
            }
          '';
          description = ''
            Declarative content for `$PASEO_HOME/config.json`. Rendered to JSON
            and installed on every service start.

            Runtime mutations to `config.json` (e.g. via `paseo daemon set-password`
            or the mobile app toggling MCP injection / provider overrides) are
            overwritten on the next restart. Pick one: manage via this option, or
            manage via the CLI — not both.

            The full schema is defined by `PersistedConfigSchema` in
            `packages/server/src/server/persisted-config.ts`.
          '';
        };
      };

      config = lib.mkIf cfg.enable (
        let
          settingsFile = (pkgs.formats.json { }).generate "paseo-config.json" cfg.settings;
        in
        {
          assertions = [
            {
              assertion = !(cfg.relay.enable && cfg.relay.mode == "remote" && cfg.relay.host == "");
              message = ''
                services.paseo.relay.host must be set when relay.mode = "remote".
              '';
            }
          ]
          ++ lib.optionals isClan [
            {
              assertion = lib.hasAttr clanEnvironmentVar config.sops.secrets;
              message = ''
                services.paseo requires the generated `paseo-auth` clan var.
                Run: clan vars generate ${machineName} --generator paseo-auth
              '';
            }
          ]
          ++ lib.optionals (!isClan) [
            {
              assertion = hasSops;
              message = ''
                services.paseo on a non-Clan machine requires the legacy sops
                setup: add `paseo-auth.password` to the machine's default sops
                file (secrets/${machineName}/secrets.yaml).
              '';
            }
          ];

          users.users.${cfg.user} = lib.mkIf (cfg.user == "paseo") {
            isSystemUser = true;
            group = cfg.group;
            home = cfg.dataDir;
          };

          users.groups.${cfg.group} = lib.mkIf (cfg.group == "paseo") { };

          systemd.tmpfiles.rules = [
            "d ${cfg.dataDir} 0700 ${cfg.user} ${cfg.group} - -"
          ];

          systemd.services.paseo = {
            description = "Paseo - self-hosted daemon for AI coding agents";
            after = [ "network.target" ];
            wantedBy = [ "multi-user.target" ];

            preStart = lib.mkIf (cfg.settings != { }) ''
              install -m 0600 ${settingsFile} ${cfg.dataDir}/config.json
            '';

            environment = {
              PASEO_HOME = cfg.dataDir;
              PASEO_LISTEN = "${cfg.listenAddress}:${toString cfg.port}";
            }
            // lib.optionalAttrs cfg.inheritUserEnvironment (
              let
                # Match dataDir's convention. We can't read users.users.<name>.home
                # because the user may be managed outside NixOS.
                userHome = "/home/${cfg.user}";
              in
              {
                # mkForce overrides the default PATH from NixOS's systemd module (which
                # only includes store paths for coreutils/grep/sed/systemd). When the
                # daemon runs as a real user, also include home-manager profile paths
                # so user-installed CLIs (claude, opencode, codex, ...) are reachable
                # by agent processes the daemon spawns.
                PATH = lib.mkForce (
                  lib.concatStringsSep ":" (
                    lib.optionals (cfg.user != "paseo") [
                      "${userHome}/.nix-profile/bin"
                      "${userHome}/.local/state/nix/profile/bin"
                    ]
                    ++ [
                      "/etc/profiles/per-user/${cfg.user}/bin"
                      "/run/current-system/sw/bin"
                      "/run/wrappers/bin"
                      "/nix/var/nix/profiles/default/bin"
                    ]
                  )
                );
              }
            )
            // lib.optionalAttrs (cfg.hostnames == true) {
              PASEO_HOSTNAMES = "true";
            }
            // lib.optionalAttrs (lib.isList cfg.hostnames && cfg.hostnames != [ ]) {
              PASEO_HOSTNAMES = lib.concatStringsSep "," cfg.hostnames;
            }
            // lib.optionalAttrs (cfg.relay.enable && cfg.relay.mode == "remote") {
              PASEO_RELAY_ENDPOINT = "${cfg.relay.host}:${toString cfg.relay.port}";
              PASEO_RELAY_USE_TLS = if cfg.relay.useTls then "true" else "false";
            }
            // lib.optionalAttrs (cfg.relay.enable && cfg.relay.mode == "remote" && cfg.relay.publicUseTls != null) {
              PASEO_RELAY_PUBLIC_USE_TLS = if cfg.relay.publicUseTls then "true" else "false";
            }
            // cfg.environment;

            serviceConfig = {
              Type = "simple";
              User = cfg.user;
              Group = cfg.group;

              ExecStart =
                "${cfg.package}/bin/paseo-server"
                + lib.optionalString (!cfg.relay.enable) " --no-relay";

              Restart = "on-failure";
              RestartSec = 5;

              # Graceful shutdown (server handles SIGTERM with a 10s timeout)
              KillSignal = "SIGTERM";
              TimeoutStopSec = 15;
            }
            // lib.optionalAttrs (isClan || hasSops) {
              # Paseo only accepts the password as a plain environment value and
              # bcrypt-hashes it on startup. Clan vars and sops both provide an
              # env-file form so systemd sees it before exec (a preStart hook
              # would be too late for EnvironmentFile).
              EnvironmentFile = [
                (if isClan then
                  config.clan.core.vars.generators.paseo-auth.files.env.path
                else
                  config.sops.templates."paseo-env".path)
              ];
            };
          };

          environment.systemPackages = [ cfg.package ];

          networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];
        }
        // lib.optionalAttrs isClan {
          clan.core.vars.generators.paseo-auth = {
            files = {
              # The readable copy is for operators (`clan vars get`, or reading
              # the deployed secret) — the service itself only consumes `env`.
              password = {
                owner = cfg.user;
                group = cfg.group;
                mode = "0400";
                restartUnits = [ "paseo.service" ];
              };
              env = {
                restartUnits = [ "paseo.service" ];
              };
            };
            runtimeInputs = [ pkgs.openssl ];
            script = ''
              password=$(openssl rand -base64 24 | tr -d '\n' | tr '+/' '-_')
              printf '%s' "$password" > "$out/password"
              printf 'PASEO_PASSWORD=%s\n' "$password" > "$out/env"
            '';
          };
        }
        // lib.optionalAttrs (!isClan && hasSops) {
          sops.secrets."paseo-auth/password" = {
            restartUnits = [ "paseo.service" ];
          };
          sops.templates."paseo-env" = {
            content = "PASEO_PASSWORD=${config.sops.placeholder."paseo-auth/password"}";
            restartUnits = [ "paseo.service" ];
          };
        }
      );
    };
}
