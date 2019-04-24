{pkgs, lib, ...}:

let haskellOverrides = self: super:
   with pkgs.haskell.lib;
   let pkg = self.callPackage; in
   rec {
     taffybar = (
       addPkgconfigDepend (disableLibraryProfiling (dontCheck (
         super.callCabal2nix
         "taffybar"
         ../taffybar
         {}
       )))
       pkgs.gtk3
     );

     broadcast-chan = pkg
       ({ mkDerivation, async, base, criterion, deepseq, stm
       , unliftio-core
       }:
       mkDerivation {
         pname = "broadcast-chan";
         version = "0.2.0.2";
         sha256 = "12ax37y9i3cs8wifz01lpq0awm9c235l5xkybf13ywvyk5svb0jv";
         libraryHaskellDepends = [ base unliftio-core ];
         benchmarkHaskellDepends = [ async base criterion deepseq stm ];
         description = "Closable, fair, single-wakeup channel type that avoids 0 reader space leaks";
         license = pkgs.stdenv.lib.licenses.bsd3;
         hydraPlatforms = pkgs.stdenv.lib.platforms.none;
         broken = false;
       }) {};
   };
in {
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghcEnv
      pkgs.taffybar
    ];

    nixpkgs.config.packageOverrides = super: rec {
      myHaskellPackages = pkgs.haskell.packages.ghc864.override {
        overrides = haskellOverrides;
      };

      taffybar = super.taffybar.override {
        ghcWithPackages = myHaskellPackages.ghcWithPackages;
      };

      xmonad-with-packages = super.xmonad-with-packages.override {
        ghcWithPackages = myHaskellPackages.ghcWithPackages;
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
        name = "ghc864";
        paths = with myHaskellPackages; [
          (myGhcWithHoogleFiltered ghcWithHoogle)
          alex
          cabal-install
          ghc-core
          # ghcid
          # taffybar
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
