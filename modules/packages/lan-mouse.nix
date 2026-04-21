{ inputs, ... }:
{
  flake-file.inputs.lan-mouse.url = "github:feschber/lan-mouse";

  flake.overlays.lan-mouse = final: prev: {
    lan-mouse = final.callPackage "${inputs.lan-mouse}/nix/default.nix" { };
  };
}
