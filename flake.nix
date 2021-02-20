{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;

  inputs.nixpkgs-master.url = github:NixOS/nixpkgs/master;

  inputs.home-manager = {
    url = github:nix-community/home-manager/release-20.09;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.taffybar = {
    url = path:./taffybar-flake;
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.emacs.url = github:nix-community/emacs-overlay/8bb502cca3b1dc3ed35d1ebeacdc92364a80997e;

  outputs = { self, nixpkgs, nixpkgs-master, home-manager, taffybar, emacs }@inputs:
  let
    system = "x86_64-linux";
    overlays = {
      bleeding = final: prev: {
        bleeding = import inputs.nixpkgs-master { inherit system; };
      };
    };
  in {
    nixosConfigurations.valak = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ./configuration.nix-valak
        home-manager.nixosModules.home-manager
        nixpkgs.nixosModules.notDetected
        { nixpkgs.overlays = [ taffybar.overlay emacs.overlay overlays.bleeding ]; }
      ];
    };
  };

}
