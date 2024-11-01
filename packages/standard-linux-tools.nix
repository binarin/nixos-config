{ pkgs, config, lib, ... }:
let
  cfg = config.standard-linux-tools;
in
{
  imports = [
    ./tmux.nix
    ../packages/perl-packages.nix
  ];
  options = {
    standard-linux-tools = {
      gitPackage = lib.mkOption {
        type = lib.types.package;
        default = pkgs.gitAndTools.gitFull;
      };
    };
  };
  config = {
    environment.systemPackages = with pkgs; [
      ov
      bat
      bind # for dig
      binutils
      bridge-utils
      broot
      cfg.gitPackage
      cryptsetup
      darcs
      dpkg
      elinks
      file
      fzf
      gdb
      gitAndTools.diff-so-fancy
      gnum4
      gnumake
      htop
      httpie
      iftop
      inetutils
      inotify-tools
      iotop
      ipcalc
      iptables
      jq
      lsof
      man-pages
      mc
      mosh
      nix-output-monitor
      nmap
      openssl
      p7zip
      parallel
      pciutils
      psmisc
      pv
      reptyr
      ripgrep
      rtorrent
      socat
      sysstat
      tcpdump
      unrar
      unzip
      usbutils
      wget
      curl
      which
      whois
      wol
      zip
      zsh
    ];

    services.locate = {
      enable = true;
      localuser = "root";
    };

    security.sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };

    security.pam.loginLimits = [
      {
        domain = "*";
        type = "-";
        item = "nofile";
        value = "131072";
      }
    ];
  };
}
