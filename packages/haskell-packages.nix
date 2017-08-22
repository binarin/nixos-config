{pkgs, lib, ...}:

{
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghc80Env
      pkgs.ghc80ProfEnv
    ];
    nixpkgs.config.packageOverrides = super: rec {
      haskellPackages = super.haskellPackages.override {
        overrides = self: super: {
          ghc-syb-utils = pkgs.haskell.lib.dontCheck super.ghc-syb-utils;
        };
      };
      haskell802Packages = super.haskell.packages.ghc802.override {
        overrides = myHaskellPackages false;
      };

      profiledHaskell802Packages = super.haskell.packages.ghc802.override {
        overrides = myHaskellPackages true;
      };

      myHaskellPackages = libProf: self: super:
        with pkgs.haskell.lib;
        let pkg = self.callPackage; in
        let reflex-src = import ./haskell-reflex-frp.nix {inherit (pkgs) fetchFromGitHub; }; in
        rec {
          clay = dontCheck super.clay;
          ghc-syb-utils = dontCheck super.ghc-syb-utils;
          encoding = appendPatch super.encoding ./haskell-encoding.patch; # remove constraint on 'binary' so it'll build at all; and ignore GB18030 as it's built time is around an hour
          reflex = pkg reflex-src.reflex {};
          mkDerivation = args: super.mkDerivation (args // {
            enableLibraryProfiling = libProf;
            enableExecutableProfiling = false;
          });
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

      ghc80Env = super.pkgs.buildEnv {
        name = "ghc80";
        paths = with haskell802Packages; [
          (myGhcWithHoogleFiltered ghcWithHoogle)
          alex
          cabal-install
          cabal2nix
          ghc-core
          ghc-mod
          hasktags
          happy
          hlint
          hpack
          stylish-haskell
        ];
      };

      ghc80ProfEnv = super.pkgs.myEnvFun {
        name = "ghc80prof";
        buildInputs = with profiledHaskell802Packages; [
          profiledHaskell802Packages.ghc
          cabal-install
          ghc-core
          hlint
          hasktags
        ];
      };

    };
  };
}
