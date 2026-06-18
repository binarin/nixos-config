{ self, ... }:
let
  selfLib = self.lib.self;
in
{
  flake.nixosModules.gui =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.gui";

      imports = [
        self.modules.generic.chrome-policies
      ];

      config = {
        programs.chromium.enable = true;
        services.avahi = lib.mkIf (!config.networking.useNetworkd) {
          enable = true;
          nssmdns4 = true;
          openFirewall = true;
        };

        services.printing = {
          enable = true;
          drivers = with pkgs; [
            cups-filters
            cups-browsed
          ];
        };

        home-manager.sharedModules = [
          self.homeModules.gui
          self.homeModules.insync
        ];

        environment.systemPackages = with pkgs; [
          appimage-run
          brightnessctl
          ddcutil
          kanshi
          wev
          wl-clipboard

          sddm-astronaut # https://github.com/NixOS/nixpkgs/issues/390251
        ];

        hardware.graphics = {
          enable = true;
          enable32Bit = true;
        };

        services.pipewire = {
          enable = true;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
        };

        services.desktopManager.plasma6.enable = true;
        services.displayManager.sddm = {
          enable = true;
          wayland.enable = true;
          extraPackages = with pkgs; [
            sddm-astronaut
          ];
          theme = "sddm-astronaut-theme";
        };

        services.flatpak.enable = true;
      };
    };

  flake.homeModules.gui =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let

      texlive-combined = pkgs.texlive.combine { inherit (pkgs.texlive) scheme-full beamer ps2eps; };

      guiPackages = with pkgs; [
        chromium
        element-desktop
        flacon
        freecad
        gitg
        imhex
        libnotify
        libreoffice
        mesa-demos
        openscad-unstable
        steam-run
        texlive-combined
        usbutils.python
        v4l-utils
        vscode
        xdg-user-dirs
        self.packages."${pkgs.stdenv.hostPlatform.system}".v4l-play
      ];
    in
    {
      key = "nixos-config.modules.home.gui";
      imports = [
        self.homeModules.xdg-autostart
        self.homeModules.desktop-essentials
      ];

      options = {
        programs.telegram-desktop.enable = lib.mkEnableOption "Enable telegram";
        programs.slack = {
          enable = lib.mkEnableOption "Enable slack";
          package = lib.mkPackageOption pkgs [ "slack" ] { };
        };
      };

      config = (
        lib.mkMerge [
          {
            programs.google-chrome.enable = lib.mkDefault true;
            programs.slack.enable = lib.mkDefault true;

            xdg.autostart.override."org.kde.xwaylandvideobridge".notShownIn = [
              "Hyprland"
              "niri"
            ];

            xdg.autostart.override."update-notifier".hidden = true;

            xdg.dataFile =
              with lib;
              (pipe "${self}/files/xdg-applications" [
                builtins.readDir
                (filterAttrs (n: v: v == "regular" && hasSuffix ".desktop" n))
                attrNames
                (flip genAttrs' (
                  fn: nameValuePair "applications/${fn}" { source = selfLib.file "xdg-applications/${fn}"; }
                ))
              ]);

            programs.thunderbird.enable = lib.mkDefault true;
            programs.telegram-desktop.enable = lib.mkDefault true;

            xdg.mimeApps = {
              enable = true;
              defaultApplications = {
                "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
              };
            };

            # xdg.configFile."mimeapps.list".force = true;

            gtk = {
              enable = true;
              gtk3.bookmarks = [
                "file://${config.home.homeDirectory}/personal-workspace"
                "file://${config.home.homeDirectory}/OneDrive/3D%20Printing"
                "file://${config.home.homeDirectory}/org"
              ];
            };
            home.packages = guiPackages;
          }
          (lib.mkIf config.programs.slack.enable {
            home.packages = [
              config.programs.slack.package
            ];
            impermanence.local-directories = [
              ".config/Slack"
            ];
          })
          (lib.mkIf config.programs.google-chrome.enable {
            impermanence.local-directories = [
              ".config/google-chrome"
            ];
          })
          (lib.mkIf config.programs.telegram-desktop.enable {
            home.packages = with pkgs; [ telegram-desktop ];
            impermanence.local-directories = [ ".local/share/TelegramDesktop" ];
          })
          (lib.mkIf config.programs.thunderbird.enable {
            programs.thunderbird.profiles = { };
            impermanence.local-directories = [ ".thunderbird" ];
          })
          ({
            home.packages = [
              (pkgs.ptouch-print.overrideAttrs {
                version = "1.6.0";
                src = pkgs.fetchgit {
                  url = "https://git.familie-radermacher.ch/linux/ptouch-print.git";
                  rev = "aa5392bc135161252d06c48745c0d53c281d69f3";
                  hash = "sha256-CG1ZituzJ5l9nuTXjk+yjsKZidoiWh6o/vArchQYsd8=";
                };
              })
            ];
          })
          {
            home.packages = [ pkgs.anki ];
            impermanence.persist-directories = [
              ".local/share/Anki2"
            ];
            impermanence.local-directories = [
              ".cache/Anki"
            ];
          }
          {
            home.packages = [ pkgs.moonlight-qt ];
            impermanence.persist-directories = [
              ".config/Moonlight Game Streaming Project"
            ];
            impermanence.local-directories = [
              ".cache/Moonlight Game Streaming Project"
            ];
          }
          {
            home.packages = [ pkgs.prusa-slicer ];
            impermanence.persist-directories = [
              ".config/PrusaSlicer"
              ".local/share/prusa-slicer"
            ];
            impermanence.local-directories = [
              ".cache/prusa-slicer"
            ];
          }
        ]
      );
    };
}
