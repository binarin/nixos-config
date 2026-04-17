{ ... }:
{
  flake.homeModules.binarin-ssh =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
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
