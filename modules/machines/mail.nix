{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.mail = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inherit self inputs;
      hostConfig = {
        isLinux = true;
      };
    };
    modules = [
      self.nixosModules.mail-configuration
    ]
    ++ self.nixosSharedModules;
  };

  flake.nixosModules.mail-configuration =
    {
      config,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.mail-configuration";
      imports = [
        self.nixosModules.default
      ];

      config = {
        networking.hostName = "mail";
        hostConfig.features = [
          "lxc"
          "tailscale"
        ];

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
      };
    };
}
