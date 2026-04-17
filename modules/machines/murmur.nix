{ self, inputs, ... }:
{
  flake-file.inputs.home-manager-unstable = {
    url = "github:nix-community/home-manager/master";
    inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

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
        self.homeModules.binarin-ssh
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
        # age-plugin-yubikey
        age
        bpftrace
        btop
        chromium
        gimp
        google-chrome
        htop
        iftop
        iotop
        kanata
        mosh
        niri
        nixfmt
        nixgl.nixGLIntel
        passage
        rclone
        restic
        ripgrep
        slack
        wl-clipboard
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

  flake.deploy.nodes.b-adb-k = {
    hostname = "adb.k.b";
    sshUser = "allebedev";
    profiles.user = {
      path = self.lib.deploy-home-manager self.homeConfigurations.b-dev-kvm;
    };
  };

  flake.homeConfigurations.b-dev-kvm = inputs.home-manager-unstable.lib.homeManagerConfiguration {
    pkgs = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;

    modules = [
      self.homeModules.b-dev-kvm-configuration
    ];

    extraSpecialArgs = {
      osConfig.services.graphical-desktop.enable = false;
      osConfig.impermanence.enable = false;
      self'.packages = self.packages.x86_64-linux;
    };
  };

  flake.homeModules.b-dev-kvm-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      imports = [
        self.homeModules.binarin-zsh
        self.homeModules.binarin-baseline
      ];

      xdg.enable = false;
      programs.zsh.dotDir = config.home.homeDirectory;

      programs.zsh.envExtra = ''
        if [[ -d $HOME/.local/bin && !( $PATH == *$HOME/.local/bin* ) ]]; then
            export PATH="$HOME/.local/bin:$PATH"
        fi

        if [[ -f $HOME/.cargo/env ]] ; then
            . "$HOME/.cargo/env"
        fi

        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
            . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi
      '';

      home.activation.we-own-zsh-configs = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
        $DRY_RUN_CMD rm -f ~/.zshrc ~/.zshenv ~/.bash_profile ~/.bashrc
      '';

      home.activation.fix-permissions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD chmod 0755 ~/.cache/oh-my-zsh/completions
      '';

      programs.starship.settings.command_timeout = 2000;

      home.stateVersion = "25.11";
      home.username = "allebedev";
      home.homeDirectory = "/home/allebedev";
      home.packages = with pkgs; [
        fd
        ripgrep
      ];
    };

}
