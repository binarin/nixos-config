{ flake, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
final: prev: {
  lnxlink = final.callPackage "${self}/packages/lnxlink.nix" { };
}
