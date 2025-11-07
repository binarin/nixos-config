{self, ...}: {
  flake.nixosModules.gui = {pkgs, config, lib, ...}: {
    config = {
      home-manager.sharedModules = [
        self.homeModules.gui
      ];
      environment.systemPackages = with pkgs; [
        sddm-astronaut
        sddm-chili-theme
        appimage-run
        brightnessctl
        ddcutil
        kanshi
        wev
        wl-clipboard
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
        theme = "sddm-astronaut-theme";
      };

      services.flatpak.enable = true;
    };
  };

  flake.homeModules.gui = { osConfig, flake, config, pkgs, lib, ... }:
    let
      ignoringVulns =
        x:
        x
        // {
          meta = x.meta // {
            knownVulnerabilities = [ ];
          };
        };
      qtwebkitIgnoringVulns = pkgs.qt5.qtwebkit.overrideAttrs ignoringVulns;

      texlive-combined = pkgs.texlive.combine { inherit (pkgs.texlive) scheme-full beamer ps2eps; };

      guiPackages = with pkgs; [
        protonmail-bridge
        chromium
        evince
        flacon
        geeqie
        gimp
        gitg
        glxinfo
        imagemagickBig
        imhex
        libnotify
        mplayer
        insync
        protonvpn-cli
        steam-run
        texlive-combined
        usbutils.python
        vlc
        vscode
        xdg-user-dirs
        xdotool
        xorg.xhost
      ];

      slowRebuildGuiPackages = with pkgs; [
        # (goldendict.override { qtwebkit = qtwebkitIgnoringVulns; })
      ];

      inherit (lib) optionals;
      inherit (config.hostConfig) feature;
    in
      {
        options = {
          programs.telegram-desktop.enable = lib.mkEnableOption "Enable telegram";
        };

        config = (lib.mkMerge [
          {
            programs.thunderbird.enable = lib.mkDefault true;
            programs.telegram-desktop.enable = lib.mkDefault true;

            xdg.mimeApps = {
              enable = true;
              defaultApplications = {
                "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
                "image/jpeg" = "geeqie.desktop";
              };
              associations.added = {
                "application/pdf" = "org.gnome.Evince.desktop";
                "image/jpeg" = "geeqie.desktop";
              };
            };
            xdg.configFile."mimeapps.list".force = true;
            gtk = {
              enable = true;
              gtk3.bookmarks = [
                ''file://${config.home.homeDirectory}/personal-workspace''
                ''file://${config.home.homeDirectory}/OneDrive/3D%20Printing''
                ''file://${config.home.homeDirectory}/org''
              ];
            };
            home.packages = guiPackages ++ (optionals (!feature.fast-rebuild) slowRebuildGuiPackages);
          }
          (lib.mkIf config.programs.telegram-desktop.enable {
            home.packages = with pkgs; [ tdesktop ];
            impermanence.local-directories = [ "${config.xdg.dataHomeRelative}/TelegramDesktop" ];
          })
          (lib.mkIf config.programs.thunderbird.enable {
            programs.thunderbird.profiles = {};
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
        ]);
      };
}
