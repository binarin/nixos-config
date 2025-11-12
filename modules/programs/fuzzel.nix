{ ... }:
{
  flake.homeModules.fuzzel =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.fuzzel";

      home.packages = with pkgs; [
        fuzzel
      ];

      impermanence.local-files = [ ".cache/fuzzel" ];

      stylix.targets.fuzzel.enable = true;
    };
}
