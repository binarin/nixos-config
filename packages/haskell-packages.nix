{pkgs, lib, ...}:

let haskellOverrides = libProf: self: super:
   with pkgs.haskell.lib;
   let pkg = self.callPackage; in
   let reflex-src = import ./haskell-reflex-frp.nix {inherit (pkgs) fetchFromGitHub; }; in
   rec {
     text-icu = dontCheck super.text-icu;
     # data-fix = super.data-fix_0_0_7;
     # clay = dontCheck super.clay;
     # ghc-syb-utils = dontCheck super.ghc-syb-utils;
     # encoding = appendPatch super.encoding ./haskell-encoding.patch; # remove constraint on 'binary' so it'll build at all; and ignore GB18030 as it's built time is around an hour
     # quickcheck-instances = doJailbreak super.quickcheck-instances;
     reflex = pkg reflex-src.reflex {};
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
      pkgs.haskellPackages.hpack-convert
      # pkgs.haskellPackages.tinc
    ];
    nixpkgs.config.packageOverrides = super: rec {
      myHaskellPackages = pkgs.bleeding.haskell.packages.ghc822.override {
        overrides = haskellOverrides false;
      };

      myProfiledHaskellPackages = super.haskell.packages.ghc822.override {
        overrides = haskellOverrides true;
      };

      myGhcWithHoogle = ghcWithHoogle: ghcWithHoogle (import ./hoogle-local-packages.nix);
      myGhcWithHoogleFiltered = ghcWithHoogle: pkgs.bleeding.stdenv.mkDerivation {
        name = "ghc-without-some-binaries";
        src = myGhcWithHoogle ghcWithHoogle;
        builder = pkgs.bleeding.writeScript "my-filtered-ghc-builder" ''
          . $stdenv/setup
          mkdir -p $out/bin
          for binary in $(find $src/bin -type f -or -type l) ; do
            if [[ ! $binary =~ (taffybar|xmonad) ]]; then
              ln -s $binary $out/bin/
            fi
          done
        '';
      };

      ghcEnv = super.pkgs.bleeding.buildEnv {
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
