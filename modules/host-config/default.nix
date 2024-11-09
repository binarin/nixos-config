{flake, lib, config, osConfig ? null, ...}:
let
  cfg = config.hostConfig;
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  options = {
    inventoryHostName = lib.mkOption {
      type = lib.types.str;
    };
    hostConfig = {
      managedUsers = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = ''
          Users that should be instantiated on this host
        '';
      };

      gui.enable = lib.mkEnableOption "Enable gui-related stuff";
      lnxlink.enable = lib.mkEnableOption "Allow this host to be controlled by Home Assistant";
      bleeding.enable = lib.mkEnableOption "Allow using unstable nixpkgs via pkgs.bleeding.<XXXX>";

      ipamConfig = lib.mkOption {
        type = lib.types.attrs;
      };
    };
  };

  config = {
    hostConfig = lib.mkDefault (self.lib.hostConfig.forHost config.inventoryHostName);
  };
}
