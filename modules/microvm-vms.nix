{ self, inputs, lib, ... }: {
  config = {
    flake.nixosModules.microvm-vms = { config, lib, pkgs, ... }:
    let
      cfg = config.nixos-config.microvms;

      # Helper function to create a microVM configuration
      mkMicroVM = { name, ip, macAddress, workspace, extraPackages ? [], extraConfig ? {} }:
        let
          vmConfig = {
            # Hostname
            networking.hostName = name;

            # Import base microvm configuration
            imports = [
              self.nixosModules.microvm-base
            ];

            # Workspace and credentials paths
            microvm.workspace = workspace;
            microvm.credentialsDir = cfg.credentialsDir;

            # Network configuration
            microvm.interfaces = [
              {
                type = "tap";
                id = "vm-${name}";
                mac = macAddress;
              }
            ];

            systemd.network.networks."10-lan" = {
              matchConfig.Name = "enp*";
              networkConfig = {
                Address = "${ip}/24";
                Gateway = "${cfg.subnet}.1";
                DNS = "${cfg.subnet}.1";
              };
              linkConfig.RequiredForOnline = "no";
            };

            # Additional packages
            environment.systemPackages = extraPackages;

            # Auto-start VM
            microvm.autostart = true;
          };
        in
        lib.recursiveUpdate vmConfig extraConfig;
    in
    {
      key = "nixos-config.modules.nixos.microvm-vms";

      config = lib.mkIf cfg.enable {
        # Example VM: claudevm
        microvm.vms.claudevm = mkMicroVM {
          name = "claudevm";
          ip = "${cfg.subnet}.2";
          macAddress = "02:00:00:00:00:02";
          workspace = "${cfg.workspaceBaseDir}/claudevm";
          extraPackages = with pkgs; [
            nodejs
            python3
          ];
        };
      };
    };
  };
}
