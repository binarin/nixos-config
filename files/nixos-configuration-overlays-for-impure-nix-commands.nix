final: prev:
let
  flake = builtins.getFlake "${builtins.readFile /etc/nix/inputs/self-absolute-path}"; # can't handle nested symlinks
  host = builtins.head (builtins.split "\n" (builtins.readFile "/etc/hostname"));
  configuration = flake.nixosConfigurations."${host}";
  configurationOverlays = configuration.config.nixpkgs.overlays;
  lib = configuration.pkgs.lib;
in
lib.composeManyExtensions configurationOverlays final prev
