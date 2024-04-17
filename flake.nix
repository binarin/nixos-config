{
  inputs = {


    nixos.url = github:NixOS/nixpkgs/nixos-23.11;

    nixpkgs-master.url = github:NixOS/nixpkgs/master;

    nixpkgs.url = github:nixos/nixpkgs/nixos-23.11;

    home-manager.url = github:nix-community/home-manager/release-23.11;
    home-manager.inputs.nixpkgs.follows = "nixos";

    hyprland.url = github:hyprwm/Hyprland/v0.39.0;
    hyprland.inputs.nixpkgs.follows = "nixos";

    hyprland-contrib.url = github:hyprwm/contrib;
    hyprland-contrib.inputs.nixpkgs.follows = "nixos";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    emacs.url = github:nix-community/emacs-overlay/master;

    cq.url = github:marcus7070/cq-flake;

    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

  };

  outputs = { self, nixos, nixpkgs, nixpkgs-master, flake-compat,
              darwin, home-manager, emacs, cq, hyprland, hyprland-contrib}@inputs:

  let
    globalOverlays = [
      emacs.overlay

      hyprland.overlays.default
      hyprland-contrib.overlays.default
      (final: prev: {
        hyprland = prev.hyprland.override {
          libdrm = final.bleeding.libdrm;
          wayland-protocols = final.bleeding.wayland-protocols;
        };
        wlroots-hyprland = prev.wlroots-hyprland.override {
          wlroots = (prev.wlroots.override {
            wayland-protocols = final.bleeding.wayland-protocols;
            mesa = prev.mesa.override {
              libdrm = final.bleeding.libdrm;
            };
          }).overrideAttrs (a: {
            buildInputs = a.buildInputs ++ [final.bleeding.hwdata final.bleeding.libdisplay-info];
          });
        };
      })

      (
        final: prev: {
          bleeding = import nixpkgs-master {
            inherit (prev) system;
            config = nixpkgsConfig;
            overlays = [
              emacs.overlay
            ];
          };

          wt-maker = final.callPackage ./packages/wt-maker.nix {};

          # NOTE: This one is picked up by home-manager emacs module
          # emacsPackagesFor = final.bleeding.emacsPackagesFor;

          # ytt = prev.ytt.overrideAttrs (oldAttrs: {
          #   src = final.fetchFromGitHub {
          #     owner = "vmware-tanzu";
          #     repo = "carvel-ytt";
          #     rev = "v0.35.1";
          #     sha256 = "sha256-hSs+kKefhth8hvR13+Lqg8lC/pvPScXAhSOtHDl8ax0=";
          #   };
          # });
          # cups = prev.cups.overrideAttrs (oldAttrs: {
          #   patchd = final.fetchpatch {
          #     name = "expiring-subscriptions.patch";
          #     url = "https://github.com/OpenPrinting/cups/commit/7225df2a2a52309dcb11922d718f79329cbdaefb.patch";
          #     sha256 = "sha256-gnii0aGND+6aS59PUcdU4WtH4uQTpQ8G1/U3WNFK97c=";
          #   };
          # });
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
        {
          home-manager.useGlobalPkgs = true;
          home-manager.backupFileExtension = "backup";
        }
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

    homeConfigurations.binarin = home-manager.lib.homeManagerConfiguration {
      configuration = {
        imports = [ ./users/binarin-hm.nix ];
        nixpkgs.overlays = globalOverlays;
        nixpkgs.config = nixpkgsConfig;
        programs.home-manager.enable = true;
      };
      system = "x86_64-linux";
      username = "binarin";
      homeDirectory = "/home/binarin";
      stateVersion = "21.11";
    };

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
