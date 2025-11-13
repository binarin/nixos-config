{ inputs, ...}: {
  flake.homeModules.syncthing = {config, ...}: {
    key = "nixos-config.modules.home.syncthing";

    disabledModules = [
      "${inputs.home-manager}/modules/services/syncthing.nix"
    ];
    imports = [
      "${inputs.home-manager-master}/modules/services/syncthing.nix"
    ];

    sops.secrets."syncthing-ui-password" = {};

    services.syncthing = {
      enable = true;
      tray.enable = true;
      passwordFile = config.sops.secrets."syncthing-ui-password".path;
      settings = {
        devices = {
          pixel8.id = "ROLQIQ6-OEAMVJA-KRM4IDA-KOGUXR6-RZW3LRR-C6VZSAF-LZL5ZEV-PWAQTQL";
        };
        folders = {
          "/home/binarin/Music" = {
            id = "demandred-music";
            devices = [ "pixel8" ];
          };
        };
      };
    };
  };
}
