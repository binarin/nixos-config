{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake) inputs;
in
{
  config = lib.mkIf config.hostConfig.feature.emacs {
    nixpkgs.overlays = [
      flake.inputs.emacs-overlay.overlays.default
      (_final: prev: {
        emacs-pgtk-saved = prev.emacs-pgtk;
        emacs-saved = prev.emacs;
        # tree-sitter = final.bleeding.tree-sitter;
      })
    ];
  };
}
