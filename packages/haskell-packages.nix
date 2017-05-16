{pkgs, ...}:

{
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghc80Env
      pkgs.ghc80ProfEnv
    ];
    nixpkgs.config.packageOverrides = super: rec {
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
          clay = dontCheck clay;
          reflex = pkg reflex-src.reflex {};
          mkDerivation = args: super.mkDerivation (args // {
            enableLibraryProfiling = libProf;
            enableExecutableProfiling = false;
          });
        };

      ghc80Env = super.pkgs.buildEnv {
        name = "ghc80";
        paths = with haskell802Packages; [
          (ghcWithHoogle (import ./hoogle-local-packages.nix))
          alex
          cabal-install
          cabal2nix
          ghc-core
          ghc-mod
          hlint
          hasktags
          happy
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
