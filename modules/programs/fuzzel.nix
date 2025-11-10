{ ... }:
{
  flake.homeModules.fuzzel =
    { pkgs, ... }:
    {
      key = "nixos-config.programs.fuzzel";

      home.packages = with pkgs; [
        fuzzel
      ];

      impermanence.local-files = [ ".cache/fuzzel" ];

      stylix.targets.fuzzel.enable = true;
    };
}
