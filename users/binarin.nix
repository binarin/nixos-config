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
    };
    nix.trustedUsers = [ "binarin" ];
  };
}
