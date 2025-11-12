{...}: {
  flake.homeModules.syncthing = {...}: {
    key = "nixos-config.modules.home.syncthing";
    services.syncthing = {
      enable = true;
    };
  };
}
