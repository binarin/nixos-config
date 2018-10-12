# because of https://github.com/taffybar/taffybar/issues/358
self: super:

let
  haskellOverrides = self: super: {
    haskell-gi-overloading = super.haskell-gi-overloading_0_0;
  };
  haskellPackages = super.haskell.packages.ghc822.override {
    overrides = haskellOverrides;
  };
in {
  taffybar = super.taffybar.override {
    inherit (haskellPackages) ghcWithPackages;
  };
}
