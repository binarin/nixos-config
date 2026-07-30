{ ... }:
{
  flake.homeModules.binarin-ssh =
    {
      pkgs,
      lib,
      config,
      osConfig,
      ...
    }:
    let
      sshHosts = {
        pi-box = {
          overrides = {
            "colors-dark.background" = "001800";
          };
        };
        murmur = {
          remoteShell = "ssh -t";
          overrides = {
            "colors-dark.background" = "001800";
          };
        };
        "db.k.b" = {
          remoteShell = "ssh -t";
          overrides = {
            "colors-dark.background" = "180000";
          };
        };
      };
    in

    {
      key = "nixos-config.modules.home.binarin-ssh";

      options.binarin-ssh.viaMurmur = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Route work traffic through murmur. Must stay false on murmur itself,
          where it would be a self-proxy loop.
        '';
      };

      config = {
        xdg.dataFile = lib.mkIf osConfig.services.graphical-desktop.enable (
          with lib;
          (flip mapAttrs') sshHosts (
            host:
            opts@{
              remoteShell ? "mosh",
              ...
            }:
            let
              overridesStr = pipe opts.overrides [
                (mapAttrsToList (k: v: "--override=${escapeShellArg k}=${escapeShellArg v}"))
                (concatStringsSep " ")
              ];
            in
            nameValuePair "applications/ssh-${host}.desktop" {
              text = ''
                [Desktop Entry]
                Name=ssh to ${host}
                Exec=foot-unique-window "SSH|${host}" ${overridesStr} -e ${remoteShell} ${host} -- tmux -u new-session -A -D -s binarin
                Type=Application
                Terminal=false
                Categories=System;
                Icon=foot
              '';
            }
          )
        );

        programs.ssh = {
          enable = true;
          enableDefaultConfig = false;

          includes = [
            # ad-hoc, unmanaged; first wins in ssh_config, so this overrides the rest
            "~/.ssh/local-config.d/*.conf"
            "/run/secrets/ssh-local-common"
            "/run/secrets/ssh-local-offsite"
          ];

          settings = {
            # Keys must carry their own "Match "/"Host " prefix: home-manager
            # only uses the key literally when it starts with one, and otherwise
            # prepends "Host ". Without it, "originalhost *.k.b" renders as
            # `Host originalhost *.k.b`, which also matches a host named
            # "originalhost".
            "Match originalhost *.k.b" = {
              ForwardAgent = true;
              ControlMaster = "auto";
              ControlPersist = "yes";
            };

            "*" = {
              # "*" is automatically sorted at the end
              ForwardAgent = false;
              AddKeysToAgent = "no";
              Compression = false;
              ServerAliveInterval = 0;
              ServerAliveCountMax = 3;
              HashKnownHosts = false;
              UserKnownHostsFile = "~/.ssh/known_hosts";
              ControlMaster = "no";
              ControlPath = "~/.ssh/master-%r@%k:%p";
              ControlPersist = "no";
            };
          }
          // lib.optionalAttrs config.binarin-ssh.viaMurmur {
            "Match originalhost murmur" = {
              ForwardAgent = true;
              IdentitiesOnly = true;
              IdentityFile = "~/.ssh/id_ed25519";
              IdentityAgent = "~/.ssh/ssh-agent-alt-1.socket";
              ControlMaster = "auto";
              ControlPersist = "yes";
              Hostname = "murmur-wifi.home.binarin.info";
              User = "allebedev";
            };

            "Match host murmur-wifi.home.binarin.info" = {
              User = "allebedev";
            };
          };
        };
      };
    };
}
