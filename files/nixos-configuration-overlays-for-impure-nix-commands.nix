final: prev:
let
  flake = with builtins; getFlake (unsafeDiscardStringContext (readFile /etc/nix/inputs/self-absolute-path));
  host = builtins.head (builtins.split "\n" (builtins.readFile "/etc/hostname"));
  configuration = flake.nixosConfigurations."${host}";
  configurationOverlays = configuration.config.nixpkgs.overlays;
  lib = configuration.pkgs.lib;
in
lib.composeManyExtensions configurationOverlays final prev
