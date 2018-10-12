# because of https://github.com/taffybar/taffybar/issues/358
self: super: rec {
  taffybar = super.taffybar.override {
    inherit (super.haskell.packages.ghc822) ghcWithPackages;
  };
}
