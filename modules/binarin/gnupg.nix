{ inputs, ... }:
{
  # The gnupg modules live in common.
  flake.nixosModules.gnupg = inputs.common.nixosModules.gnupg;
  flake.homeModules.gnupg = inputs.common.homeModules.gnupg;
}
