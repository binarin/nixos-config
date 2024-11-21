{flake, config, lib, pkgs, ...}:
let
  # One of the Windows 95 default colors
  wallpaper = pkgs.runCommand "image.png" {} ''
    ${pkgs.imagemagick}/bin/magick -size 1920x1080 "xc:#00807F" $out
  '';
in
{
  imports = [
    flake.inputs.stylix.nixosModules.stylix
  ];

  disabledModules = [
    "${flake.inputs.stylix}/modules/regreet/nixos.nix"
  ];

  options = {
  };

  config = lib.mkMerge [
    {
      stylix.homeManagerIntegration.followSystem = false;
      stylix.homeManagerIntegration.autoImport = false;
    }
    (lib.mkIf config.hostConfig.feature.gui {
      environment.systemPackages = with pkgs; [
        fontpreview
        inter
      ];
      stylix.targets.lightdm.enable = true;
      stylix.targets.gtk.enable = true;
      stylix.targets.chromium.enable = true;
    })
  ];
}
