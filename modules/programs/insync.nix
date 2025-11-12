{ ... }:
{
  flake.homeModules.insync =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.insync";

      home.packages = with pkgs; [
        insync
      ];

      impermanence.local-directories = [
        "Insync"
        ".config/Insync"
        ".local/share/Insync"
        ".cache/Insync"
      ];
    };
}
