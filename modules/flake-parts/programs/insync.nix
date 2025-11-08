{ ... }:
{
  flake.homeModules.insync =
    { pkgs, ... }:
    {

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
