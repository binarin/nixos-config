{self, ...}: {
  flake.homeModules.fuzzel = {pkgs, config, ...}: {
    key = "nixos-config.programs.fuzzel";

    home.packages = with pkgs; [
      fuzzel
    ];

    impermanence.local-files = [ "${config.xdg.cacheHomeRelative}/fuzzel" ];

    stylix.targets.fuzzel.enable = true;
  };
}
