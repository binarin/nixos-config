{
  system ? builtins.currentSystem,
  config ? {
    allowUnfree = true;
  },
  overlays ? (import ./default.nix).overlays,
  inNixShell ? false
}:
  import (import ./default.nix).inputs.nixpkgs { inherit system config overlays;  }
