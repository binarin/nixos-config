{
  inputs = {
    nixos.url = github:NixOS/nixpkgs/nixos-21.05;
    nixpkgs-master.url = github:NixOS/nixpkgs/master;
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-21.05-darwin;

    home-manager.url = github:nix-community/home-manager/release-21.05;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    taffybar.url = github:taffybar/taffybar/master;
    taffybar.flake = false;

    emacs.url = github:nix-community/emacs-overlay/master;

    cq.url = github:marcus7070/cq-flake;

    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    comma = { url = "github:Shopify/comma"; flake = false; };
  };

  outputs = { self, nixos, nixpkgs, nixpkgs-master, flake-compat,
              darwin, home-manager, taffybar, emacs, cq, comma}@inputs:

  let
    taffybar-overlay = (
      self: super:
      let
        taffybarOverlay = _: pkgs: rec {
          haskellPackages = pkgs.haskellPackages.override (old: {
            overrides =
              pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
              (self: super: {
                taffybar =
                  self.callCabal2nix "taffybar" taffybar
                  { inherit (pkgs) gtk3; };
              });
          });
        };
      in super.lib.composeExtensions taffybarOverlay (import "${taffybar}/environment.nix") self super
    );

    xmonad-config-overlay = final: prev: {
      inherit (import ./xmonad-config/default.nix { pkgs = final; }) my-xmonad-config my-xmonad-executable;
    };

    globalOverlays = [
      emacs.overlay
      taffybar-overlay
      xmonad-config-overlay
      (
        final: prev: {
          bleeding = import nixpkgs-master {
            inherit (prev) system;
            config = nixpkgsConfig;
            overlays = [ emacs.overlay ];
          };
          emacsPackagesFor = final.bleeding.emacsPackagesFor;
          comma = import comma { inherit (prev) pkgs; };

          ytt = prev.ytt.overrideAttrs (oldAttrs: {
            src = final.fetchFromGitHub {
              owner = "vmware-tanzu";
              repo = "carvel-ytt";
              rev = "v0.35.1";
              sha256 = "sha256-hSs+kKefhth8hvR13+Lqg8lC/pvPScXAhSOtHDl8ax0=";
            };
          });
        }
      )
    ];

    nixpkgsConfig = {
      allowUnfree = true;
      oraclejdk.accept_license = true;
    };

    nixCommonConfigModule = {pkgs, ...}: {
      nixpkgs = rec {
        config = nixpkgsConfig;
        overlays = globalOverlays;
      };
      nix = {
        package = pkgs.nixUnstable;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';

        nixPath = [
          "nixpkgs=${./.}/nixpkgs.nix"
        ];
      };
    };

    linuxSystem = configuration: nixos.lib.nixosSystem {
      system = "x86_64-linux";
	    modules = [
	      configuration
        nixCommonConfigModule
	      home-manager.nixosModules.home-manager
	      nixos.nixosModules.notDetected
        { home-manager.useGlobalPkgs = true; }
	    ];
    };

  in rec {
    overlays = globalOverlays;

    nixosConfigurations.valak = linuxSystem ./configuration.nix-valak;
    nixosConfigurations.nix-build = linuxSystem ./configuration.nix-nix-build;

    darwinConfigurations.vmware-laptop = darwin.lib.darwinSystem {
      modules = [
        home-manager.darwinModules.home-manager
        ./users/binarin-fonts.nix
        nixCommonConfigModule
        (
          {pkgs, ... }: {
            services.nix-daemon.enable = true;

            users.users.alebedeff.home = "/Users/alebedeff";

            programs.zsh.enable = true; # Make `nix-darwin` play together with `zsh`

            environment.systemPackages = with pkgs; [
              coreutils
              procps
              findutils
            ];

            home-manager.useGlobalPkgs = true;

            home-manager.users.alebedeff = {
              imports = [
                ./users/binarin-hm.nix
              ];
            };

            fonts.enableFontDir = true;
          }
        )
      ];
    };
  };
}
