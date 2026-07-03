{
  self,
  inputs,
  lib,
  ...
}:
let
  selfLib = self.lib.self;
  makeSystemConfig = (import "${self}/lib/make-system-config.nix" {
    inherit lib;
    nixos = "${inputs.nixpkgs}/nixos";
    userborn = inputs.system-manager.inputs.userborn;
    system-manager-src = inputs.system-manager;
  }).makeSystemConfig;

  murmurOverlays = [
    inputs.nixgl.overlay
    inputs.system-manager.overlays.default
    (final: prev: {
      swaylock = final.writeShellScriptBin "swaylock" ''
        exec /usr/bin/swaylock "$@"
      '';
    })
  ];
  murmurPkgs = self.configured-pkgs.x86_64-linux.nixpkgs.appendOverlays murmurOverlays;
in
{
  flake-file.inputs.nixgl = {
    url = "github:nix-community/nixgl";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.lib.murmurPkgs = murmurPkgs;
  flake.lib.murmurOverlays = murmurOverlays;

  flake.systemConfigs.murmur = makeSystemConfig {
    pkgs = murmurPkgs;
    modules = [
      self.systemModules.bubuntu
      self.systemModules.sshd
      self.systemModules.sops
      self.systemModules.home-manager
      (
        { pkgs, lib, ... }:
        {
          services.openssh.managedUsers = [ "root" "allebedev" "binarin" ];
          users.users.allebedev = {
            isNormalUser = true;
            group = "allebedev";
            shell = lib.mkForce pkgs.zsh;
            ignoreShellProgramCheck = true;
            openssh.authorizedPrincipals = [ "allebedev" "binarin" "root" ];
          };
        users.users.binarin = {
          isNormalUser = true;
          group = "binarin";
          openssh.authorizedPrincipals = [ "binarin" ];
        };
        users.groups.allebedev = { };
        users.groups.binarin = { };

        programs.uwsm.waylandCompositors.niri = {
          prettyName = "niri";
          comment = "niri scrollable-tiling Wayland compositor";
          execCommand = "nixGLIntel \${UWSM_BIN} start -N niri -D niri -C niri -e -- \"$(which niri-session)\"";
          preExec = ''
            __HM_SESS_VARS_SOURCED=
            __ETC_PROFILE_NIX_SOURCED=
            . /etc/profile.d/nix.sh
            . /home/allebedev/.profile
            export XDG_DATA_DIRS="''${XDG_DATA_DIRS:+$XDG_DATA_DIRS:}/usr/local/share:/usr/share"
            . /etc/profile.d/zz-prefer-nix-paths.sh
          '';
        };

        bubuntu.apt.packages = [ "swaylock" ];

        programs.chromium.extraOpts.AutoLaunchProtocolsFromOrigins = [
          {
            allowed_origins = [ "*" ];
            protocol = "globalprotectcallback";
          }
        ];

        sops.defaultSopsFile = selfLib.file' "secrets/murmur/secrets.yaml";
        sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

        sops.secrets.nix-extra-access-tokens = { };

        # Pin corporate security agents to a single E-core (CPU 12)
        environment.etc = let
          corporateBloatDropin = ''
            [Service]
            Slice=corporate-bloat.slice
            Nice=19
            CPUSchedulingPolicy=idle
            IOSchedulingPriority=7
            IOSchedulingClass=idle
          '';
        in {
          "systemd/system/corporate-bloat.slice".text = ''
            [Slice]
            AllowedCPUs=12
          '';
          "systemd/system/falcon-sensor.service.d/cpu-pin.conf".text = corporateBloatDropin;
          "systemd/system/nix.service.d/cpu-pin.conf".text = corporateBloatDropin;
          "systemd/system/nessusagent.service.d/limit.conf".text = corporateBloatDropin;
        };

        systemd.services.mask-ubuntu-updater = {
          wantedBy = [ "system-manager.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            /usr/bin/systemctl mask \
              apt-daily.timer \
              apt-daily-upgrade.timer \
              apt-daily.service \
              apt-daily-upgrade.service \
              update-notifier-download.timer \
              update-notifier-download.service \
              unattended-upgrades.service
            /usr/bin/systemctl stop \
              apt-daily.timer \
              apt-daily-upgrade.timer \
              update-notifier-download.timer \
              unattended-upgrades.service \
              2>/dev/null || true
          '';
        };

        systemd.services.kanata = {
          serviceConfig = {
            Type = "simple";
            Restart = "on-failure";
            RestartSec = 1;
            ExecStart = "${pkgs.kanata}/bin/kanata --cfg ${selfLib.file "kanata.config"}";
          };
        };
        }
      )
      ({ config, ... }: {
        sops.templates."nix-custom-conf" = {
          path = "/etc/nix/nix.custom.conf";
          content = ''
            extra-access-tokens = ${config.sops.placeholder.nix-extra-access-tokens}
            trusted-users = root allebedev
          '';
        };
      })
      ({ lib, ... }: {
        home-manager.useGlobalPkgs = true;
        home-manager.backupFileExtension = "backup";
        home-manager.sharedModules = [
          self.homeModules.home-misc
        ];
        home-manager.extraSpecialArgs = {
          self'.packages = self.packages.x86_64-linux;
          inputs' =
            with lib;
            pipe inputs [
              (filterAttrs (_: v: v ? packages))
              (mapAttrs (
                _: v: {
                  packages = v.packages.x86_64-linux;
                }
              ))
            ];
        };
        home-manager.users.allebedev = self.homeModules.murmur-home-allebedev;

        services.graphical-desktop.enable = true;
        networking.hostName = "murmur";
      })
    ];
  };

  flake.deploy.nodes.murmur = {
    hostname = "murmur";
    sshUser = "allebedev";
    profiles.system = {
      user = "root";
      path = self.lib.deploy-system-manager self.systemConfigs.murmur;
    };
  };

  flake.homeModules.murmur-home-allebedev =
    {
      lib,
      pkgs,
      config,
      inputs',
      ...
    }:
    let
      globalprotect-callback = pkgs.writeShellApplication {
        name = "globalprotect-callback";
        runtimeInputs = [ pkgs.systemd ];
        text = ''
          echo "$(date): called with: $*" >> /tmp/globalprotect-callback.log
          systemd-run --user --collect --quiet \
            /usr/bin/globalprotect defaultbrowser "$1"
          echo "$(date): systemd-run exit=$?" >> /tmp/globalprotect-callback.log
        '';
      };
    in
    {
      key = "nixos-config.modules.home.murmur-home-allebedev";
      imports = [
        self.homeModules.emacs
        self.homeModules.foot
        self.homeModules.firefox
        self.homeModules.niri
        self.homeModules.direnv
        self.homeModules.xdg-autostart
        self.homeModules.binarin-baseline
        self.homeModules.binarin-ssh
        self.homeModules.standalone-home-manager-zsh
        self.homeModules.desktop-essentials
      ];

      services.ssh-agent.enable = true;
      home.packages =
        (with pkgs; [
          jerk-gpa
          ksso
          klaude
          # age-plugin-yubikey
          lan-mouse
          age
          bpftrace
          btop
          chromium
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
          xdg-utils
          xwayland-satellite
          go_1_26
          tramp-rpc-server
        ])
        ++ (with pkgs.llm-agents; [
          claude-code
          workmux
        ]);

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
        Icon=${pkgs.slack}/share/pixmaps/slack.png
      '';


      xdg.mimeApps.defaultApplications."x-scheme-handler/slack" = "slack.desktop";
      xdg.mimeApps.defaultApplications."x-scheme-handler/globalprotectcallback" = "globalprotectcallback.desktop";

      xdg.dataFile."applications/globalprotectcallback.desktop".text = ''
        [Desktop Entry]
        Name=GlobalProtect Callback
        Exec=${lib.getExe globalprotect-callback} %u
        Type=Application
        NoDisplay=true
        MimeType=x-scheme-handler/globalprotectcallback;
      '';

      # xdg.autostart.override."remotesupport".notShownIn = [
      #   "niri"
      # ];

      programs.zsh.dotDir = "${config.xdg.configHome}/zsh";

      xdg.autostart.override."epp-client".hidden = true;
      xdg.autostart.override."org.gnome.DejaDup.Monitor".hidden = true;
      xdg.autostart.override."snap-userd-autostart".hidden = true;

      xdg.configFile."autostart/google-chrome.desktop".text = ''
        [Desktop Entry]
        Name=Google Chrome
        Exec=${lib.getExe pkgs.google-chrome}
        Type=Application
        OnlyShowIn=niri;
      '';

      home.activation.mask-unwanted-user-services = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD /usr/bin/systemctl --user mask \
          snap.snapd-desktop-integration.snapd-desktop-integration.service \
          update-notifier.service
      '';

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
