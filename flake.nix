{
  inputs = {
    nixos.url = github:NixOS/nixpkgs/nixos-20.09;
    nixpkgs-master.url = github:NixOS/nixpkgs/master;
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-21.05-darwin;

    home-manager.url = github:nix-community/home-manager/release-21.05;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    taffybar.url = path:./taffybar-flake;
    taffybar.inputs.nixpkgs.follows = "nixos";

    emacs.url = github:nix-community/emacs-overlay/master;

    cq.url = github:marcus7070/cq-flake;
  };

  outputs = { self, nixos, nixpkgs, nixpkgs-master,
              darwin, home-manager, taffybar, emacs, cq}@inputs:
  let
    nixpkgsConfig = rec {
      config = { allowUnfree = true; };
      overlays = [
        emacs.overlay
        (
          final: prev: rec {
            bleeding = import nixpkgs-master { inherit (prev) system; inherit config; };
            emacsPackagesFor = final.bleeding.emacsPackagesFor;
          }
        )
      ];
    };
    system = "x86_64-linux";
    overlays = {
      bleeding = final: prev: {
        bleeding = import inputs.nixpkgs-master {
          inherit system;
          overlays = [ emacs.overlay ];
        };
      };
    };
  in rec {
    rawConfigurations = {
      valak = {
	      inherit system;
	      modules = [
	        ./configuration.nix-valak
	        home-manager.nixosModules.home-manager
	        nixos.nixosModules.notDetected
	        { nixpkgs.overlays = [ taffybar.overlay emacs.overlay overlays.bleeding ]; }
	        { environment.systemPackages = [ cq.packages."${system}".cq-editor ]; }
	      ];
      };
      nix-build = {
	      inherit system;
	      modules = [
	        ./configuration.nix-nix-build
	        home-manager.nixosModules.home-manager
	        nixos.nixosModules.notDetected
	        { nixpkgs.overlays = [ taffybar.overlay emacs.overlay overlays.bleeding ]; }
	      ];
      };
    };

    nixosConfigurations.valak = nixos.lib.nixosSystem rawConfigurations.valak;
    nixosConfigurations.nix-build = nixos.lib.nixosSystem rawConfigurations.nix-build;

    homeManagerConfigurations = {
      darwin = inputs.home-manager.lib.homeManagerConfiguration {
        configuration = {
          imports = [
            ./users/binarin-hm.nix
          ];
          nixpkgs.overlays = [ emacs.overlay overlays.bleeding ];
        };
	      system = "x86_64-darwin";
	      homeDirectory = "/Users/alebedeff";
	      username = "alebedeff";
	      pkgs = import nixpkgs-master {
          system = "x86_64-darwin";
        };
      };
    };

    darwinConfigurations.vmware-laptop = darwin.lib.darwinSystem {
      modules = [
        home-manager.darwinModules.home-manager
        (
          {pkgs, ... }: {
            services.nix-daemon.enable = true;

            nix = {
              package = pkgs.nixUnstable;
              extraOptions = ''
                experimental-features = nix-command flakes
              '';
            };

            nixpkgs = nixpkgsConfig;

            users.users.alebedeff.home = "/Users/alebedeff";

            home-manager.useGlobalPkgs = true;

            home-manager.users.alebedeff = {
              imports = [
                ./users/binarin-hm.nix
              ];
            };
          }
        )
      ];
    };
  };
}
