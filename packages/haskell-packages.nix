{pkgs, lib, ...}:

let haskellOverrides = self: super:
   with super.haskell.lib;
   let pkg = self.callPackage; in
   rec {
   };
in {
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghcEnv
    ];

    nixpkgs.config.packageOverrides = super: rec {
      myHaskellPackages = pkgs.haskell.packages.ghc844.override {
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
        name = "ghc844";
        paths = with myHaskellPackages; [
          (myGhcWithHoogleFiltered ghcWithHoogle)
          alex
          cabal-install
          ghc-core
          # ghcid
          happy
          hasktags
          hlint
          hpack
          hspec
          stylish-haskell
        ];
      };
    };
  };
}
