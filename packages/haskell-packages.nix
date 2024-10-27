{ pkgs, lib, ... }:

let
  haskellOverrides = self: super:
    with pkgs.haskell.lib;
    let
      pkg = self.callPackage;
      markUnbroken = drv: overrideCabal drv (drv: { broken = false; });
    in
    rec {
      # clay = dontCheck (markUnbroken super.clay);
    };
in
{
  options = { };
  config = {
    environment.systemPackages = [
      pkgs.ghcEnv
    ];

    nixpkgs.config.packageOverrides = super: rec {
      myHaskellPackages = pkgs.bleeding.haskell.packages.ghc902.override {
        overrides = haskellOverrides;
      };

      myGhcWithHoogle = ghcWithHoogle: ghcWithHoogle (import ./hoogle-local-packages.nix);

      ghcEnv = super.pkgs.buildEnv {
        name = "ghc902";
        paths = with myHaskellPackages; [
          (myGhcWithHoogle ghcWithHoogle)
          alex
          cabal-install
          cabal2nix
          haskell-language-server
          ghc-core
          happy
          hasktags
          # hlint
          hpack
          hspec
          ghcid.bin
        ];
      };
    };
  };
}
