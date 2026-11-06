{ inputs, ... }:
{
  flake.overlays.lan-mouse = final: prev: {
    lan-mouse = final.callPackage "${inputs.lan-mouse}/nix/default.nix" { };
  };
}
