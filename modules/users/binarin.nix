{ self, ... }:
let

  nixosHome = "/home/binarin";

in
{
  flake.nixosModules.user-binarin =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.user-binarin";

      imports = [
        self.nixosModules.home-manager
        self.nixosModules.impermanence
        self.nixosModules.stylix
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
            home.homeDirectory = "/home/binarin";
            home.username = "binarin";
            home.stateVersion = config.system.stateVersion;
          };
        };
    };

  flake.homeModules.user-binarin =
    { lib, osConfig, ... }:
    {
      key = "nixos-config.modules.home.user-binarin";

      imports = [
        self.homeModules.impermanence
        self.homeModules.git
        self.homeModules.emacs
        self.homeModules.claude-code
        self.homeModules.direnv
        self.homeModules.interactive-cli
        self.homeModules.sops
      ]
      ++ (lib.optionals osConfig.services.graphical-desktop.enable [
        self.homeModules.niri
        # self.homeModules.hyprland
        self.homeModules.wezterm
        self.homeModules.foot
        self.homeModules.fonts
        self.homeModules.syncthing
      ]);

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
