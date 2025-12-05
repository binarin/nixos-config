{ ... }:
{
  flake.nixosModules.tailscale =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.services.tailscale;
      serveCfg = cfg.serve;
    in
    {
      key = "nixos-config.modules.nixos.tailscale";

      options = {
        services.tailscale.serve = {
          enable = lib.mkEnableOption "Tailscale serve configuration management";

          configs = lib.mkOption {
            type = lib.types.attrsOf (
              lib.types.submodule {
                options = {
                  protocol = lib.mkOption {
                    type = lib.types.enum [
                      "http"
                      "https"
                      "tcp"
                      "tls-terminated-tcp"
                    ];
                    default = "https";
                    description = "Protocol type for this serve configuration";
                  };

                  target = lib.mkOption {
                    type = lib.types.str;
                    description = ''
                      Target backend. Examples:
                      - Port: "3000"
                      - Host:port: "localhost:3000"
                      - Full URL: "http://localhost:3000"
                      - Insecure HTTPS: "https+insecure://localhost:8443"
                    '';
                    example = "localhost:3000";
                  };

                  port = lib.mkOption {
                    type = lib.types.nullOr lib.types.port;
                    default = null;
                    description = "Port to expose on tailnet (for HTTP/HTTPS/TCP protocols)";
                  };

                  path = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = null;
                    description = "Path to append to base URL (--set-path flag)";
                    example = "/api";
                  };
                };
              }
            );
            default = { };
            description = "Named serve configurations to apply";
            example = lib.literalExpression ''
              {
                web = {
                  protocol = "https";
                  target = "localhost:3000";
                };
                api = {
                  protocol = "https";
                  target = "localhost:8080";
                  path = "/api";
                };
              }
            '';
          };

          services = lib.mkOption {
            type = lib.types.attrsOf (
              lib.types.submodule {
                options = {
                  serviceName = lib.mkOption {
                    type = lib.types.str;
                    description = ''
                      Service name to advertise on the tailnet (without svc: prefix).
                      The svc: prefix will be automatically added.
                    '';
                    example = "archivebox";
                  };

                  protocol = lib.mkOption {
                    type = lib.types.enum [
                      "http"
                      "https"
                      "tcp"
                      "tls-terminated-tcp"
                    ];
                    default = "https";
                    description = "Protocol type for this service configuration";
                  };

                  target = lib.mkOption {
                    type = lib.types.str;
                    description = ''
                      Target backend. Examples:
                      - Port: "3000"
                      - Host:port: "localhost:3000"
                      - Full URL: "http://localhost:3000"
                      - Insecure HTTPS: "https+insecure://localhost:8443"
                    '';
                    example = "localhost:8000";
                  };

                  port = lib.mkOption {
                    type = lib.types.nullOr lib.types.port;
                    default = null;
                    description = "Port to expose on tailnet service";
                  };

                  path = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = null;
                    description = "Path to append to base URL (--set-path flag)";
                    example = "/api";
                  };

                  tunnel = lib.mkOption {
                    type = lib.types.bool;
                    default = false;
                    description = "Forward all traffic to the local machine (--tun flag)";
                  };
                };
              }
            );
            default = { };
            description = "Named tailscale service configurations (with distinct virtual IPs)";
            example = lib.literalExpression ''
              {
                archivebox = {
                  serviceName = "archivebox";
                  protocol = "https";
                  target = "localhost:8000";
                };
              }
            '';
          };
        };
      };

      config = lib.mkMerge [
        # Existing tailscale config
        {
          services.tailscale = {
            enable = true;
            extraUpFlags = [
              "--hostname"
              "${config.networking.hostName}"
            ];
          };

          # tailscale can't use exit nodes otherwise
          # citing https://github.com/tailscale/tailscale/issues/4432#issuecomment-1112819111:
          #  Blindly applying strict RPF to all traffic just doesn't work
          #  any more in modern network environments, especially desktops
          #  which usually have multiple active interfaces at once and
          #  complex things like Tailscale going on.
          networking.firewall.checkReversePath = lib.mkForce "loose";
        }

        # New serve configuration service
        (lib.mkIf (serveCfg.enable && (serveCfg.configs != { } || serveCfg.services != { })) {
          systemd.services.tailscale-serve-config = {
            description = "Configure Tailscale serve";

            # Dependencies - match tailscaled-set pattern from nixpkgs
            after = [
              "tailscaled.service"
            ]
            ++ lib.optional (cfg.authKeyFile != null) "tailscaled-autoconnect.service";
            bindsTo = [ "tailscaled.service" ];
            wantedBy = [ "multi-user.target" ];

            # Restart on configuration changes
            restartTriggers = [
              (builtins.toJSON serveCfg.configs)
              (builtins.toJSON serveCfg.services)
            ];

            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
            };

            path = [
              cfg.package
              pkgs.jq
            ];

            script = ''
              set -euo pipefail

              # Wait for tailscale to be ready
              echo "Waiting for tailscale to be ready..."
              for i in {1..30}; do
                if tailscale status >/dev/null 2>&1; then
                  echo "Tailscaled is ready"
                  break
                fi
                if [ $i -eq 30 ]; then
                  echo "Timeout waiting for tailscaled"
                  exit 1
                fi
                sleep 1
              done

              # Get current serve configuration
              current=$(tailscale serve status --json)

              # Build desired configuration JSON representation
              desired=$(cat <<'EOF'
              {"configs":${builtins.toJSON serveCfg.configs},"services":${builtins.toJSON serveCfg.services}}
              EOF
              )

              # Simple comparison - if current matches our stored config hash, skip update
              # We use a state file to track the last applied config
              state_file="/var/lib/tailscale-serve-config.json"

              if [ -f "$state_file" ] && [ "$(cat "$state_file")" = "$desired" ] && [ "$current" != "{}" ]; then
                echo "Configuration unchanged, skipping update"
                exit 0
              fi

              # Configuration changed or first run, apply changes
              echo "Configuration changed, updating serves..."

              # Reset all serves
              tailscale serve reset || {
                echo "Warning: Failed to reset serve config, continuing anyway"
              }

              # Apply each configured serve (per-node)
              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (name: srvCfg: ''
                  echo "Configuring serve: ${name}"
                  tailscale serve --bg \
                    ${lib.optionalString (srvCfg.port != null) "--${srvCfg.protocol}=${toString srvCfg.port}"} \
                    ${lib.optionalString (srvCfg.path != null) "--set-path=${srvCfg.path}"} \
                    ${srvCfg.target} \
                    || { echo "Failed to configure ${name}"; exit 1; }
                '') serveCfg.configs
              )}

              # Apply each configured service (with distinct virtual IP)
              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (name: svcCfg: ''
                  echo "Configuring service: ${name} (svc:${svcCfg.serviceName})"
                  tailscale serve \
                    --service=svc:${svcCfg.serviceName} \
                    ${lib.optionalString (svcCfg.port != null) "--${svcCfg.protocol}=${toString svcCfg.port}"} \
                    ${lib.optionalString (svcCfg.path != null) "--set-path=${svcCfg.path}"} \
                    ${lib.optionalString svcCfg.tunnel "--tun"} \
                    ${svcCfg.target} \
                    || { echo "Failed to configure service ${name}"; exit 1; }
                '') serveCfg.services
              )}

              # Save current config to state file
              echo "$desired" > "$state_file"

              echo "Serve configuration applied successfully"
              tailscale serve status
            '';
          };
        })
      ];
    };
}
