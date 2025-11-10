{ self, ... }:
let

  nixosHome = "/home/binarin";

in
{
  flake.nixosModules.user-binarin =
    { config, lib, ... }:
    {
      key = "nixos-config.users.binarin";

      imports = [
        self.nixosModules.home-manager
      ];

      users.users = {
        binarin = {
          description = "Alexey Lebedeff";
          uid = 1000;
          isNormalUser = true;
          group = "binarin";
          home = nixosHome;
          shell = "/run/current-system/sw/bin/zsh";
          extraGroups = [
            "dialout"
            "docker"
            "i2c"
            "libvirtd"
            "lxd"
            "networkmanager"
            "transmission"
            "tss" # for TPM2
            "users"
            "vboxusers"
            "video"
            "wheel"
            "wireshark"
          ];
          openssh = {
            authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "binarin";
            authorizedPrincipals = [
              "root"
              "binarin"
            ];
          };
        };
      };

      users.groups = {
        binarin = {
          gid = 1000;
        };
      };

      nix.settings.trusted-users = [ "binarin" ];

      programs.zsh.enable = true;

      home-manager.users.binarin =
        { ... }:
        {
          imports = [
            self.homeModules.user-binarin
          ];
          config = {
            inherit (config) hostConfig;
            home.homeDirectory = "/home/binarin";
            home.username = "binarin";
            home.stateVersion = config.system.stateVersion;
          };
        };
    };

  flake.homeModules.user-binarin =
    { ... }:
    {
      key = "nixos-config.users.binarin";

      imports = [
        self.homeModules.claude-code
        self.homeModules.direnv
        self.homeModules.emacs
      ];

      programs.git = {
        userName = "Alexey Lebedeff";
        userEmail = "binarin@binarin.info";
      };

      impermanence.local-files = [
      ];

      impermanence.local-directories = [
        ".cache/nixos-config"
        ".cache/nix"
        ".local/state/home-manager"
        ".local/state/nix"
      ];

      impermanence.persist-files = [
      ];

      impermanence.persist-directories = [
        "personal-workspace"
        "org"
        "annex"
        "finance"
        ".ssh"
      ];
    };
}
