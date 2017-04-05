{pkgs, ...}:

{
  options = {};
  config = {
    environment.systemPackages = [
      pkgs.ghc80Env
    ];
    nixpkgs.config.packageOverrides = super: rec {
      haskell802Packages = super.haskell.packages.ghc802.override {
      };

      ghc80Env = super.pkgs.myEnvFun {
        name = "ghc80";
        buildInputs = with haskell802Packages; [
          (ghcWithHoogle (import ./hoogle-local-packages.nix))
          cabal-install
          ghc-core
          ghc-mod
        ];
      };
    };
  };
}
