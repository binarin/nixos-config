{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ flake.inputs.stylix.nixosModules.stylix ];

  disabledModules = [ "${flake.inputs.stylix}/modules/regreet/nixos.nix" ];

  options = { };

  config = lib.mkMerge [
    {
      stylix.homeManagerIntegration.followSystem = false;
      stylix.homeManagerIntegration.autoImport = false;
    }
    (lib.mkIf config.hostConfig.feature.gui {
      # XXX stylix.targets.lightdm.enable = true;
      stylix.targets.gtk.enable = true;

      # XXX Another copy in home/stylix.nix
      stylix.cursor = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Amber";
        size = 16;
      };
      # XXX stylix.targets.chromium.enable = true;
    })
  ];
}
