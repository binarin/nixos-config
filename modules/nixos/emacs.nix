{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf config.hostConfig.feature.emacs {
    nixpkgs.overlays = [
      (final: prev: {
        emacs-pgtk-nixpkgs = prev.emacs-pgtk;
      })
      flake.inputs.emacs-overlay.overlays.default
      # (final: prev: {
      #   tree-sitter = final.bleeding.tree-sitter;
      # })
    ];
  };
}
