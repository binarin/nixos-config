{lib, ...}:

let
  path = ../overlay;
  content = builtins.readDir path;
in {

  # For nixos-rebuild
  nixpkgs.overlays = map (n: import (path + ("/" + n)))
     (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
       (lib.attrNames content));

}
