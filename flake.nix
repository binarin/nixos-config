{
  inputs = {


    nixos.url = github:NixOS/nixpkgs/nixos-24.05;

    nixpkgs-master.url = github:NixOS/nixpkgs/master;

    nixpkgs.url = github:nixos/nixpkgs/nixos-24.05;

    caddy-cloudflare.url = github:binarin/caddy-with-plugins;
    caddy-cloudflare.inputs.nixpkgs.follows = "nixpkgs"; #  golang < 1.23 - see https://github.com/nix-community/gomod2nix/issues/117

    home-manager.url = github:nix-community/home-manager/release-24.05;
    home-manager.inputs.nixpkgs.follows = "nixos";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs-master";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    hyprland = {
      url = "https://github.com/hyprwm/Hyprland";
      ref = "refs/tags/v0.43.0";
      type = "git";
      submodules = true;
      # inputs.nixpkgs.follows = "nixpkgs-master";
    };

    arion.url = github:hercules-ci/arion;
    arion.inputs.nixpkgs.follows = "nixpkgs";

    hyprland-contrib.url = github:hyprwm/contrib;
    hyprland-contrib.inputs.nixpkgs.follows = "nixos";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    emacs.url = github:nix-community/emacs-overlay/master;

    cq.url = github:marcus7070/cq-flake;

    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };

  outputs = { self, nixos, nixpkgs, nixpkgs-master, flake-compat, deploy-rs, sops-nix,
              darwin, home-manager, emacs, cq, caddy-cloudflare, arion,
              hyprland, hyprland-contrib }@inputs:

  let
    globalOverlays = [
      emacs.overlay
      (final: prev: { caddy-cloudflare = caddy-cloudflare.packages.${prev.system}.default; })

      # hyprland.overlays.default
      # hyprland-contrib.overlays.default

      # (final: prev: {
      #   hyprland = prev.hyprland.override {
      #     libdrm = final.bleeding.libdrm;
      #     wayland-protocols = final.bleeding.wayland-protocols;
      #   };
      #   wlroots-hyprland = prev.wlroots-hyprland.override {
      #     wlroots = (prev.wlroots.override {
      #       wayland-protocols = final.bleeding.wayland-protocols;
      #       mesa = prev.mesa.override {
      #         libdrm = final.bleeding.libdrm;
      #       };
      #     }).overrideAttrs (a: {
      #       buildInputs = a.buildInputs ++ [final.bleeding.hwdata final.bleeding.libdisplay-info];
      #     });
      #   };
      # })

      (
        final: prev: {
          bleeding = import nixpkgs-master {
            inherit (prev) system;
            config = nixpkgsConfig;
            overlays = [
              emacs.overlay
              (bf: bp: { mesa = final.mesa; })
            ];
          };

          grafana-victoriametrics-datasource = final.callPackage ./packages/victoriametrics-datasource.nix {};

          # wt-maker = final.callPackage ./packages/wt-maker.nix {};

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
        # package = pkgs.nixUnstable;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';

        nixPath = [
          "nixpkgs=${./.}/nixpkgs.nix"
        ];
      };
    };

    unmodifiedPkgs = import nixpkgs {
      system = "x86_64-linux";
      config = nixpkgsConfig;
      overlays = globalOverlays;
    };

    deployPkgs = import nixpkgs {
      system = "x86_64-linux";
      config = nixpkgsConfig;
      overlays = globalOverlays ++ [
        deploy-rs.overlay
        (self: super: { deploy-rs = { inherit (unmodifiedPkgs) deploy-rs; lib = super.deploy-rs.lib; }; })
      ];
    };

    linuxSystem = configuration: nixos.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
	    modules = [
	      configuration
        nixCommonConfigModule
	      home-manager.nixosModules.home-manager
	      nixos.nixosModules.notDetected
        sops-nix.nixosModules.sops
        arion.nixosModules.arion
        {
          home-manager.useGlobalPkgs = true;
          home-manager.backupFileExtension = "backup";
        }
	    ];
    };

    proxmoxLXCSystem = configuration: nixos.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
	    modules = [
	      configuration
        nixCommonConfigModule
	      home-manager.nixosModules.home-manager
	      nixos.nixosModules.notDetected
        sops-nix.nixosModules.sops
        arion.nixosModules.arion
        {
          home-manager.useGlobalPkgs = true;
          home-manager.backupFileExtension = "backup";
        }
        ({pkgs, modulesPath, ...}: {
          imports = [
            (modulesPath + "/virtualisation/proxmox-lxc.nix")
          ];
          proxmoxLXC.enable = true;
        })
	    ];
    };

  in rec {
    # overlays = { default = globalOverlays; };

    nixosConfigurations.valak = linuxSystem ./configuration.nix-valak;
    # nixosConfigurations.nix-build = linuxSystem ./configuration.nix-nix-build;
    # nixosConfigurations.fusion-vm = linuxSystem ./configuration.nix-fusion-vm;
    # nixosConfigurations.ishamael = linuxSystem ./configuration.nix-ishamael;
    nixosConfigurations.fileserver = proxmoxLXCSystem ./configuration.nix-fileserver;
    nixosConfigurations.monitor = proxmoxLXCSystem ./configuration.nix-monitor;

    deploy.nodes.fileserver = {
      hostname = "192.168.2.79";
      profiles.system = {
        sshUser = "root";
        path = deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations.fileserver;
      };
    };

    deploy.nodes.monitor = {
      hostname = "192.168.2.2";
      profiles.system = {
        sshUser = "root";
        path = deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations.monitor;
      };
    };

    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

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
  };
}
