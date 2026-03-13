{
  self,
  inputs,
  config,
  ...
}:
{
  flake.deploy.nodes.nix-cache = {
    hostname = config.inventory.ipAllocation."nix-cache".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.nix-cache;
    };
  };

  flake.nixosConfigurations.nix-cache = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.nix-cache-configuration
    ];

  };

  flake.nixosModules.nix-cache-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.nix-cache-configuration";
      imports = [
        "${inputs.srvos}/nixos/roles/nix-remote-builder.nix"

        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.impure-nix-setup
      ];

      config = {
        networking.hostName = "nix-cache";
        nixos-config.export-metrics.enable = true;

        system.stateVersion = "24.11";

        # LXC provisioning configuration
        proxmoxLXC = {
          cores = 16;
          memory = 24576; # 24GB
          mounts = [
            {
              mountPoint = "/nix";
              size = "512G";
            }
            {
              mountPoint = "/var/lib/private/gitea-runner";
              size = "32G";
            }
          ];
        };

        nix.settings.system-features = [
          "big-parallel"
        ];

        nix.extraOptions = ''
          build-dir = /nix/build
        '';

        users.users.nix-remote-builder.openssh.authorizedPrincipals = lib.forEach [
          "nix-remote-builder"
          "binarin"
          "root"
        ] (k: ''restrict,command="nix-daemon --stdio" ${k}'');
        roles.nix-remote-builder.schedulerPublicKeys = [ ];

        sops.secrets."nixos-config-runner-token" = {
          restartUnits = [
            ''gitea-runner-nixos\x2dconfig.service''
            ''gitea-runner-nixos\x2dconfig\x2d2.service''
          ];
        };

        sops.templates.nixos-config-runner-token-env-file.content = ''
          TOKEN=${config.sops.placeholder."nixos-config-runner-token"}
        '';

        services.gitea-actions-runner = {
          package = pkgs.forgejo-runner;
          instances =
            let
              commonConfig = {
                enable = true;
                url = "https://forgejo.lynx-lizard.ts.net";
                labels = [ "native:host" ];
                hostPackages = with pkgs; [
                  bash
                  coreutils
                  curl
                  forgejo-cli
                  gawk
                  gitMinimal
                  git-crypt
                  gnused
                  jq
                  just
                  nix
                  nodejs
                  wget
                ];
              };
            in
            {
              nixos-config = commonConfig // {
                name = config.networking.hostName;
                tokenFile = config.sops.templates.nixos-config-runner-token-env-file.path;
              };
              nixos-config-2 = commonConfig // {
                name = "${config.networking.hostName}-2";
                tokenFile = config.sops.templates.nixos-config-runner-token-env-file.path;
              };
            };
        };
      };
    };
}
