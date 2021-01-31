{config, pkgs, lib, ...}:
{
  imports = [
    ../home-manager/nixos
    ../packages/user-packages.nix
  ];
  config = {
    users.extraUsers = {
      binarin = {
        description = "Alexey Lebedeff";
        uid = 1000;
        isNormalUser = true;
        shell = "/run/current-system/sw/bin/zsh";
        extraGroups = [ "networkmanager" "docker" "libvirtd" "wheel" "dialout" "vboxusers" "wireshark" "transmission" "lxd" "video" ];
      };
    };
    programs.zsh.enable = true;
    programs.zsh.zsh-autoenv = {
      enable = true;
      package = pkgs.zsh-autoenv;
    };
    programs.zsh.ohMyZsh = {
      enable = true;
      plugins = [
        "colored-man-pages"
        "dirpersist"
      ];
      theme = "gianu";
    };
    programs.bash.enableCompletion = true;
    programs.zsh.interactiveShellInit = ''
      PATH=$PATH:${pkgs.autojump}/bin
      . ${pkgs.autojump}/share/autojump/autojump.zsh
    '';

    services.autorandr.enable = true;
    home-manager.users.binarin = {
      home.packages = config.userPackages;
      programs.direnv.enable = true;
      programs.direnv.enableNixDirenvIntegration = true;
      programs.direnv.enableZshIntegration = true;

      home.file.".xmonad/build" = {
        text = ''
          #!${pkgs.bash}/bin/bash
          set -euo pipefail

          nix build --no-link -f '<nixpkgs>' my-xmonad-config

          rm -f $1
          nix build -f '<nixpkgs>' -o $1 my-xmonad-executable
        '';
        force = true;
        executable = true;
      };

      xresources.properties = {
        "URxvt.termName" = "rxvt-unicode-256color";
        "URxvt.font" = "xft:Iosevka-22";
        "URxvt.boldFont" = "";
        "URxvt.italicFont" = "";
        "URxvt.boldItalicFont" = "";
        "URxvt.foreground" = "grey";
        "URxvt.background" = "black";
        "URxvt.colorUL" = "#86a2be";
        "URxvt.colorIT" = "red";
        "URxvt.underlineColor" = "orange";
        "URxvt.highlightColor" = "magenta";
        "URxvt.highlightTextColor" = "cyan";
        "URxvt.fading" = "30";
        "URxvt.fadeColor" = "black";
        "URxvt.reverseVideo" = false;
        "URxvt.pointerColor" = "red";
        "URxvt.pointerColor2" = "cyan";
        "URxvt.color12" = "#5555FF";
        "URxvt.urgentOnBell" = true;
        "URxvt.visualBell" = true;
        "URxvt.scrollBar" = false;
        "URxvt.perl-ext" = "filter_title";
        "URxvt.perl-ext-common" = "default,matcher,font-size,selection-to-clipboard";
        "URxvt.url-launcher" = "xdg-open";

        "URxvt.keysym.C-Up" = "font-size:increase";
        "URxvt.keysym.C-Down" = "font-size:decrease";

        "xscreensaver.newLoginCommand" = "dm-tool switch-to-greeter";
        "xscreensaver.timeout" = "5";
        "xscreensaver.lock" = true;
        "xscreensaver.lockTimeout" = "1";
        "xscreensaver.splash" = false;
        "xscreensaver.fade" = false;
        "xscreensaver.mode" = "blank";
        "xscreensaver.dpmsEnabled" = true;
        "xscreensaver.dpmsQuickOff" = true;
      };

      programs.autorandr = {
        enable = true;
        hooks = {
          postswitch = {
            taffy = ''${pkgs.procps}/bin/pkill -f taffy'';
            bh = ''${pkgs.feh}/bin/feh --bg-scale ${./green_3-wallpaper-5120x1440.jpg}'';
          };
        };
        profiles = {
          home-split = {
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c05aa000a0a0a0465030203500a9504100001a000000fd0032641ea02a000a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a20200182020311f044105a405b2309070783010000565e00a0a0a0295030203500a9504100001a584d00b8a1381440f82c4500a9504100001e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c2";
              HDMI-A-0 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-0 = {
                enable = true;
                crtc = 3;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
              };
              DisplayPort-0 = {
                enable = true;
                crtc = 2;
                mode = "1920x1080";
                position = "1920x0";
                rate = "60.00";
              };
              DisplayPort-1 = {
                enable = true;
                primary = true;
                crtc = 0;
                mode = "2560x1440";
                position = "0x1080";
                rate = "59.95";
              };
            };
          };
          home-gaming = {
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c074d600a0f038404030203a00a9504100001a000000fd0032781ebe61000a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a202002f102031ef046105a405b3f5c2309070783010000e305c000e60605018b7312565e00a0a0a0295030203500a9504100001a584d00b8a1381440f82c4500a9504100001e1a6800a0f0381f4030203a00a9504100001af4b000a0f038354030203a00a9504100001a0000000000000000000000000000000000000000000000000028701279000003013c57790188ff139f002f801f009f055400020009006c370108ff139f002f801f009f0545000200090033b70008ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f390";
              HDMI-A-0 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-0 = {
                enable = true;
                crtc = 3;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.00";
                primary = true;
              };
              DisplayPort-0 = {
                enable = true;
                crtc = 2;
                mode = "1920x1080";
                position = "1920x0";
                rate = "60.00";
              };
              DisplayPort-1 = {
                enable = false;
              };
            };
          };
          single = {
            fingerprint = {
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c01a6800a0f0381f4030203a00a9504100001a000000fd00324b1e5a2f000a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a2020029d02031af042105a2309070783010000e305c000e60605018b7312584d00b8a1381440f82c4500a9504100001e565e00a0a0a0295030203500a9504100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ac701279000003011433b70088ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009590";
              HDMI-A-0 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-0 = {
                enable = false;
              };
              DisplayPort-0 = {
                enable = false;
              };
              DisplayPort-1 = {
                enable = true;
                primary = true;
                crtc = 0;
                mode = "5120x1440";
                position = "0x1080";
                rate = "59.98";
              };
            };
          };
          home = {
            fingerprint = {
              DisplayPort-0 = "00ffffffffffff0038a316680000000028150104a5331d78e25ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0031583331363230374e420a20200114020313c1469004031f13122309070783010000011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018023a80d072382d40102c4580fe1f1100001e0000000000000000000000000000000000000000000000000000000000000000000000004d";
              DisplayPort-1 = "00ffffffffffff004c2d9c0f3056574331390104b57722783aa2a1ad4f46a7240e5054bfef80714f810081c08180a9c0b3009500d1c01a6800a0f0381f4030203a00a9504100001a000000fd00324b1e5a2f000a202020202020000000fc00433439524739780a2020202020000000ff0048345a4d4330303437330a2020029d02031af042105a2309070783010000e305c000e60605018b7312584d00b8a1381440f82c4500a9504100001e565e00a0a0a0295030203500a9504100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ac701279000003011433b70088ff139f002f801f009f0528000200090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009590";
              HDMI-A-0 = "00ffffffffffff0038a31568010101012c14010380331d78ea5ea5a2554da026115054bfef8081008140818081c095009040b300a9c0023a801871382d40582c4500fe1f1100001e000000fd00384c1f5311000a202020202020000000fc004541323332574d690a20202020000000ff0030593130313539334e420a2020012c02010400011d007251d01e206e285500fe1f1100001e8c0ad08a20e02d10103e9600fe1f110000188c0ad090204031200c405500fe1f11000018011d00bc52d01e20b8285540fe1f1100001e023a80d072382d40102c4580fe1f1100001e00000000000000000000000000000000000000000000000000000000000000000003";
            };
            config = {
              HDMI-A-0 = {
                enable = true;
                crtc = 3;
                mode = "1920x1080";
                position = "640x0";
                rate = "60.00";
              };
              DisplayPort-0 = {
                enable = true;
                crtc = 2;
                mode = "1920x1080";
                position = "2560x0";
                rate = "60.00";
              };
              DisplayPort-1 = {
                enable = true;
                primary = true;
                crtc = 0;
                mode = "5120x1440";
                position = "0x1080";
                rate = "59.98";
              };
            };
          };
        };
      };
    };
    nix.trustedUsers = [ "binarin" ];
  };
}
