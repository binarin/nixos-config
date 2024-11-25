{inputs, ...}: {
  perSystem = {
    self',
    pkgs,
    ...
  }: {
    # For 'nix fmt'
    formatter = pkgs.nixpkgs-fmt;
  };
}
