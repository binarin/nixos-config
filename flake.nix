{
  inputs.nixos.url = github:NixOS/nixpkgs/nixos-20.09;

  inputs.nixpkgs-master.url = github:NixOS/nixpkgs/master;

  inputs.home-manager = {
    url = github:nix-community/home-manager/release-20.09;
    inputs.nixpkgs.follows = "nixos";
  };

  inputs.taffybar = {
    url = path:./taffybar-flake;
    inputs.nixpkgs.follows = "nixos";
  };

  inputs.emacs.url = github:nix-community/emacs-overlay/8bb502cca3b1dc3ed35d1ebeacdc92364a80997e;

  inputs.cq.url = github:marcus7070/cq-flake;

  outputs = { self, nixos, nixpkgs-master, home-manager, taffybar, emacs, cq }@inputs:
  let
    system = "x86_64-linux";
    overlays = {
      bleeding = final: prev: {
        bleeding = import inputs.nixpkgs-master { inherit system; };
      };
    };
  in rec {
    rawConfigurations.valak = {
      inherit system;
      modules = [
        ./configuration.nix-valak
        home-manager.nixosModules.home-manager
        nixos.nixosModules.notDetected
        { nixpkgs.overlays = [ taffybar.overlay emacs.overlay overlays.bleeding ]; }
        { environment.systemPackages = [ cq.packages."${system}".cq-editor ]; }
      ];
    };
    nixosConfigurations.valak = nixos.lib.nixosSystem rawConfigurations.valak;
  };
}
