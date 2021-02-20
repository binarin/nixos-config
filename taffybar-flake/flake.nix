{
  inputs.taffybar = {
    url = github:taffybar/taffybar/master;
    flake = false;
  };

  outputs = { self, taffybar }: {
    overlay = self: super: let
      sourceTransformer = if builtins.getEnv "CI" == "" then builtins.fetchGit else (x: x);
      taffybarOverlay = _: pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override (old: {
          overrides =
            pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
            (self: super: {
              taffybar =
                self.callCabal2nix "taffybar" taffybar # (sourceTransformer taffybar)
                { inherit (pkgs) gtk3; };
            });
        });
      };
    in super.lib.composeExtensions taffybarOverlay (import "${taffybar}/environment.nix") self super;
  };
}
