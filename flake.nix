{
  inputs = {

    nixos.url = github:NixOS/nixpkgs/nixos-21.11;
    nixpkgs-master.url = github:NixOS/nixpkgs/master;
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-21.11-darwin;

    home-manager.url = github:nix-community/home-manager/master;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    taffybar.url = github:taffybar/taffybar/master;
    taffybar.inputs.nixpkgs.follows = "nixpkgs";

    emacs.url = github:nix-community/emacs-overlay/master;

    cq.url = github:marcus7070/cq-flake;

    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

  };

  outputs = { self, nixos, nixpkgs, nixpkgs-master, flake-compat,
              darwin, home-manager, taffybar, emacs, cq}@inputs:

  let
    xmonad-config-overlay = final: prev: {
      inherit (import ./xmonad-config/default.nix { pkgs = final; }) my-xmonad-config my-xmonad-executable my-taffybar;
    };

    # NIX_GHC support, should be not needed when dyre will upgrage past 0.9.1
    taffybar-dyre-patch-overlay = (
      final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: {})) (s: super: {
            dyre = prev.haskell.lib.appendPatch super.dyre (
              final.fetchpatch {
                url = "https://github.com/willdonnelly/dyre/commit/c7f29d321aae343d6b314f058812dffcba9d7133.patch";
                sha256 = "10m22k35bi6cci798vjpy4c2l08lq5nmmj24iwp0aflvmjdgscdb";
              }
            );
          });
        });
      }
    );

    globalOverlays = [
      emacs.overlay
      taffybar.overlay
      taffybar-dyre-patch-overlay
      xmonad-config-overlay
      (
        final: prev: {
          bleeding = import nixpkgs-master {
            inherit (prev) system;
            config = nixpkgsConfig;
            overlays = [
              emacs.overlay
              taffybar.overlay
              taffybar-dyre-patch-overlay
              xmonad-config-overlay
            ];
          };

          # NOTE: This one is picked up by home-manager emacs module
          emacsPackagesFor = final.bleeding.emacsPackagesFor;

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

    nixosConfigurations.valak = linuxSystem {
      disabledModules = [
        "system/boot/loader/systemd-boot/systemd-boot.nix"
      ];
      imports = [
        "${nixpkgs-master.outPath}/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix"
        ./configuration.nix-valak
      ];
    };

    nixosConfigurations.nix-build = linuxSystem ./configuration.nix-nix-build;
    nixosConfigurations.fusion-vm = linuxSystem ./configuration.nix-fusion-vm;
    nixosConfigurations.ishamael = linuxSystem ./configuration.nix-ishamael;

    darwinConfigurations.vmware-laptop = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
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

            environment.variables.LANG = "en_US.UTF-8";

            home-manager.users.alebedeff = {
              imports = [
                ./users/binarin-hm.nix
              ];
            };

            fonts.enableFontDir = true;
            nix = {
              binaryCaches = [
                "https://cache.nixos.org"
                "https://nixcache.reflex-frp.org"
              ];

              binaryCachePublicKeys = [
                "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
              ];

              useSandbox = false;
            };
          }
        )
      ];
    };
  };
}
