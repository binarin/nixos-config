{ self, ... }:
{
  flake.nixosModules.user-binarin =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.user-binarin";

      imports = [
        self.nixosModules.binarin-baseline
        self.nixosModules.home-manager
        self.nixosModules.impermanence
        self.nixosModules.stylix
        self.nixosModules.gnupg
      ];

      users.users.binarin.extraGroups = [
        "dialout"
        "docker"
        "i2c"
        "libvirtd"
        "lxd"
        "networkmanager"
        "plugdev" # QMK
        "transmission"
        "tss" # for TPM2
        "users"
        "vboxusers"
        "video"
        "wheel"
        "wireshark"
      ];

      hardware.keyboard.qmk.enable = true;

      home-manager.users.binarin = self.homeModules.user-binarin;
    };

  flake.homeModules.user-binarin =
    {
      lib,
      osConfig,
      pkgs,
      ...
    }:
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
        self.homeModules.gnupg
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

      home.packages = with pkgs; [
        gopass
      ];

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
        ".local/share/gopass"
        ".config/gopass"
      ];
    };
}
