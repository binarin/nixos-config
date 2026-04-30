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
            "colors.background" = "001800";
          };
        };
        murmur = {
          remoteShell = "ssh -t";
          overrides = {
            "colors.background" = "001800";
          };
        };
        "db.k.b" = {
          remoteShell = "ssh -t";
          overrides = {
            "colors.background" = "180000";
          };
        };
      };
    in

    {
      key = "nixos-config.modules.home.binarin-ssh";

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
          "~/.ssh/local-config.d/*.conf"
        ];

        matchBlocks = {
          "originalhost *.k.b" = {
            forwardAgent = true;
            controlMaster = "auto";
            controlPersist = "yes";
          };

          "*" = {
            # "*" is automtically sorted at the end
            extraOptions = {
              ForwardAgent = "no";
              AddKeysToAgent = "no";
              Compression = "no";
              ServerAliveInterval = "0";
              ServerAliveCountMax = "3";
              HashKnownHosts = "no";
              UserKnownHostsFile = "~/.ssh/known_hosts";
              ControlMaster = "no";
              ControlPath = "~/.ssh/master-%r@%k:%p";
              ControlPersist = "no";
            };
          };
        };
      };
    };
}
