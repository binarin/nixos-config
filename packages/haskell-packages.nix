{pkgs, lib, ...}:

let haskellOverrides = self: super:
  with pkgs.haskell.lib;
  let pkg = self.callPackage;
    markUnbroken = drv: overrideCabal drv (drv: { broken = false; });
  in rec {
    # clay = dontCheck (markUnbroken super.clay);
  };
in {
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghcEnv
    ];

    nixpkgs.config.packageOverrides = super: rec {
      myHaskellPackages = pkgs.bleeding.haskell.packages.ghc884.override {
        overrides = haskellOverrides;
      };

      myGhcWithHoogle = ghcWithHoogle: ghcWithHoogle (import ./hoogle-local-packages.nix);
      myGhcWithHoogleFiltered = ghcWithHoogle: pkgs.stdenv.mkDerivation {
        name = "ghc-without-some-binaries";
        src = myGhcWithHoogle ghcWithHoogle;
        builder = pkgs.writeScript "my-filtered-ghc-builder" ''
          . $stdenv/setup
          mkdir -p $out/bin
          for binary in $(find $src/bin -type f -or -type l) ; do
            if [[ ! $binary =~ (taffybar|xmonad) ]]; then
              ln -s $binary $out/bin/
            fi
          done
        '';
      };

      ghcEnv = super.pkgs.buildEnv {
        name = "ghc883";
        paths = with myHaskellPackages; [
          (myGhcWithHoogleFiltered ghcWithHoogle)
          alex
          cabal-install
          cabal2nix
          haskell-language-server
          ghc-core
          happy
          hasktags
          hlint
          hpack
          hspec
        ];
      };
    };
  };
}
