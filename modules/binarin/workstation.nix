{ self, ... }:
{
  flake.nixosModules.binarin-workstation =
    { ... }:
    {
      key = "nixos-config.modules.nixos.binarin-workstation";

      imports = [
        self.nixosModules.binarin-baseline
        self.nixosModules.home-manager
        self.nixosModules.impermanence
        self.nixosModules.stylix
        self.nixosModules.gnupg
        self.nixosModules.emacs
        self.nixosModules.binarin-nix-dev
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

      home-manager.users.binarin = self.homeModules.binarin-workstation;
    };

  flake.homeModules.binarin-workstation =
    {
      lib,
      osConfig,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.binarin-workstation";

      imports = [
        self.homeModules.impermanence
        self.homeModules.git
        self.homeModules.emacs
        self.homeModules.ai-tools
        self.homeModules.direnv
        self.homeModules.interactive-cli
        self.homeModules.sops
        self.homeModules.gnupg
        self.homeModules.binarin-nix-dev
      ]
      ++ (lib.optionals osConfig.services.graphical-desktop.enable [
        self.homeModules.niri
        # self.homeModules.hyprland
        self.homeModules.wezterm
        self.homeModules.foot
        self.homeModules.fonts
        self.homeModules.syncthing
        self.homeModules.firefox
      ]);

      programs.git = {
        settings.user = {
          name = "Alexey Lebedeff";
          email = "binarin@binarin.info";
        };
      };

      home.packages = with pkgs; [
        gopass
        dos2unix
        sox
        forgejo-cli
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
        ".config/qmk"
      ];
    };
}
