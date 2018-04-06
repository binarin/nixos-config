{pkgs, lib, ...}:

let haskellOverrides = libProf: self: super:
   with pkgs.haskell.lib;
   let pkg = self.callPackage; in
   rec {
     mkDerivation = args: super.mkDerivation (args // {
       enableLibraryProfiling = libProf;
       enableExecutableProfiling = false;
     });
   };
in {
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghcEnv
      # pkgs.haskellPackages.hpack-convert
    ];
    nixpkgs.config.packageOverrides = super: rec {
      myHaskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellOverrides false;
      };

      myProfiledHaskellPackages = super.haskell.packages.ghc822.override {
        overrides = haskellOverrides true;
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
        name = "ghc82";
        paths = with myHaskellPackages; [
          (myGhcWithHoogleFiltered ghcWithHoogle)
          alex
          cabal-install
          ghc-core
          ghcid
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
