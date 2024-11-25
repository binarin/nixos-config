final: prev: let
  flake = builtins.getFlake "path:${builtins.readFile /etc/nix/inputs/self-absolute-path}";
  host = builtins.head (builtins.split "\n" (builtins.readFile "/etc/hostname"));
  configuration = flake.nixosConfigurations."${host}";
  configurationOverlays = configuration.config.nixpkgs.overlays;
  lib = configuration.pkgs.lib;
in
  lib.composeManyExtensions configurationOverlays final prev
