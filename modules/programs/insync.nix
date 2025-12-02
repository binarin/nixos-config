{ ... }:
{
  flake.homeModules.insync =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.insync";

      home.packages = with pkgs; [
        insync
      ];

      impermanence.persist-directories = [
        "Insync"
        ".config/Insync"
        ".local/share/Insync"
      ];

      impermanence.local-directories = [
        ".cache/Insync"
      ];
    };
}
