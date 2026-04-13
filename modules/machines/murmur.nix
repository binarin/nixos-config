{ self, inputs, ... }:
{
  flake-file.inputs.nixgl = {
    url = "github:nix-community/nixgl";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  imports = [
    inputs.home-manager.flakeModules.home-manager
  ];

  flake.homeConfigurations.murmur = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
    modules = [
      self.homeModules.murmur-home-allebedev
    ];

    extraSpecialArgs = {
      osConfig.services.graphical-desktop.enable = true;
      osConfig.impermanence.enable = false;
      self'.packages = self.packages.x86_64-linux;
    };
  };

  flake.homeModules.murmur-home-allebedev =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    {
      imports = [
        self.homeModules.emacs
        self.homeModules.foot
        self.homeModules.firefox
        self.homeModules.niri
        self.homeModules.direnv
        self.homeModules.xdg-autostart
        self.homeModules.binarin-baseline
      ];

      nixpkgs = {
        config.allowUnfree = true;
        overlays = [
          inputs.emacs-overlay.overlays.default
          inputs.nixgl.overlay
          self.overlays.my-emacs
          self.overlays.waybar-org-clock
          self.overlays.sicstus-manual
          inputs.niri.overlays.default
          (final: prev: {
            # bubblewrap = final.writeShellScriptBin "bwrap" ''
            #   exec /usr/bin/bwrap "$@"
            # '';
            swaylock = final.writeShellScriptBin "swaylock" ''
              exec /usr/bin/swaylock "$@"
            '';
            slack = final.writeShellScriptBin "slack" ''
              # /snap/slack/current/usr/bin/slack --enable-features=UseOzonePlatform --ozone-platform=wayland "$@"
              ${lib.getExe prev.slack} --no-sandbox "$@"
            '';
            nix = inputs.determinate.packages.x86_64-linux.default;
          })
        ];
      };

      services.ssh-agent.enable = true;
      home.packages = with pkgs; [
        rclone
        restic
        passage
        age
        wl-clipboard
        # age-plugin-yubikey
        niri
        kanata
        nixgl.nixGLIntel
        nixfmt
        ripgrep
        google-chrome
        chromium
        slack
        xwayland-satellite
      ];

      xdg.dataFile."applications/niri.desktop".text = ''
        [Desktop Entry]
        Name=Niri
        Comment=A scrollable-tiling Wayland compositor
        Exec=nixGLIntel niri-session
        Type=Application
        DesktopNames=niri
      '';

      xdg.dataFile."applications/slack.desktop".text = ''
        [Desktop Entry]
        Name=Slack
        Exec=${lib.getExe pkgs.slack} %U
        MimeType=x-scheme-handler/slack;
        Type=Application

      '';

      xdg.mimeApps.defaultApplications."x-scheme-handler/slack" = "slack.desktop";

      # xdg.autostart.override."remotesupport".notShownIn = [
      #   "niri"
      # ];

      xdg.autostart.override."nvidia-settings-autostart".notShownIn = [
        "niri"
      ];

      xdg.enable = true;
      xdg.mime.enable = true;
      xdg.mimeApps.enable = true;

      programs.git = {
        settings.user = {
          name = "Alexey Lebedeff";
          email = "binarin@binarin.info";
        };
      };

      dconf.enable = lib.mkForce false; # XXX no dbus or something

      nix.package = null;

      home.username = "allebedev";
      home.homeDirectory = "/home/allebedev";
      home.stateVersion = "25.11"; # Please read the comment before changing.
      programs.home-manager.enable = true;

      services.restic = {
        enable = true;
        backups = {
          localbackup = {
            paths = [
              "/home/allebedev/org"
              "/home/allebedev/.passage"
            ];
            repository = "/backup/local";
            passwordFile = "/home/allebedev/.config/restic/restic-password";
            initialize = true;
          };
          gdrive = {
            paths = [
              "/home/allebedev/org"
              "/home/allebedev/.passage"
            ];
            repository = "rclone:gdrive:";
            passwordFile = "/home/allebedev/.config/restic/restic-password";
            timerConfig = {
              OnCalendar = "10:20";
              RandomizedDelaySec = "5h";
            };
          };
        };
      };
    };
}
