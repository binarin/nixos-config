{ self, inputs, ... }:
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
        self.nixosModules.age-encryption
        self.nixosModules.ai-tools
        self.nixosModules.devenv
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

      # nixpkgs.config.permittedInsecurePackages = [
      #   "qtwebengine-5.15.19"
      # ];

      # nixpkgs.overlays = [
      #   self.overlays.my-google-chrome
      # ];
    };

  flake.homeModules.binarin-workstation =
    {
      lib,
      osConfig,
      pkgs,
      inputs',
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
        self.homeModules.age-encryption
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
        (pkgs.callPackage "${self}/packages/aws-workspaces/package.nix" { })
        globalprotect-openconnect
        # aws-workspaces
        swi-prolog
        scryer-prolog
        gopass
        dos2unix
        sox
        inputs.nixpkgs-unstable.legacyPackages."${pkgs.stdenv.hostPlatform.system}".forgejo-cli
        inputs'.niks3.packages.niks3
        # inputs.nixpkgs-unstable.legacyPackages."${pkgs.stdenv.hostPlatform.system}".devenv
      ];

      impermanence.local-files = [
      ];

      impermanence.local-directories = [
        ".cache/nix"
        ".local/state/home-manager"
        ".local/state/nix"
        ".local/share/forgejo-cli"
      ];

      impermanence.persist-files = [
        {
          file = ".git-credentials";
          method = "symlink";
        }
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

      systemd.user.services.ssh-agent-alt-1 = {
        Install.WantedBy = [ "default.target" ];
        Unit = {
          Description = "SSH authentication agent - alternative no. 1";
          Documentation = "man:ssh-agent(1)";
        };
        Service = {
          ExecStart = "${lib.getExe' pkgs.openssh "ssh-agent"} -D -a %h/.ssh/ssh-agent-alt-1.socket";
          SuccessExitStatus = 2;
        };
      };
    };
}
