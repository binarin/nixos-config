{
  self,
  config,
  lib,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.deploy.nodes.mail = {
    hostname = config.inventory.ipAllocation."mail".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.mail;
    };
  };

  clan.inventory.machines.mail = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.mail.home.primary.address;
  };

  clan.machines.mail = {
    imports = [
      self.nixosModules.mail-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.mail = lib.mkForce (
    self.clan.nixosConfigurations.mail.extendModules {
      specialArgs.inventoryHostName = "mail";
    }
  );

  flake.nixosModules.mail-configuration =
    {
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.mail-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
      ];

      config = {
        networking.hostName = "mail";
        system.stateVersion = "24.05";

        # tailscale serve --bg --tls-terminated-tcp 1143 1143
        # tailscale serve --bg --tls-terminated-tcp 1025 1025
        # gpg --pinentry-mode loopback --quick-gen-key --passphrase '' 'Mail LXC <mail-lxc@binarin.info>'

        environment.systemPackages = with pkgs; [
          imapsync
          protonmail-bridge
          gnupg
          pinentry-tty
          pass
        ];

        systemd.services.protonmail-bridge = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            User = "protonmail-bridge";
          };
          path = with pkgs; [
            protonmail-bridge
            gnupg
            pass
          ];
          script = ''
            protonmail-bridge -n
          '';
        };

        users.users.protonmail-bridge = {
          isNormalUser = true;
          group = "protonmail-bridge";
        };
        users.groups.protonmail-bridge = { };

        nixos-config.export-metrics.enable = true;
      };
    };
}
