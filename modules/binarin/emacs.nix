{ inputs, ... }:
{
  # Emacs package, config and modules live in common.
  flake.overlays.my-emacs = inputs.common.overlays.my-emacs;
  flake.nixosModules.emacs = inputs.common.nixosModules.emacs;
  flake.homeModules.emacs = inputs.common.homeModules.emacs;
}
