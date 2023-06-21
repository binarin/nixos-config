{pkgs, config, lib, ...}:
let
  cfg = config.standard-linux-tools;
in
{
  imports = [
    ../modules/tmux.nix
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
      reptyr
      apg
      bat
      bind # for dig
      binutils
      bridge-utils
      broot
      cryptsetup
      darcs
      dpkg
      elinks
      file
      fzf
      gdb
      gitAndTools.diff-so-fancy
      gitAndTools.git-annex
      gitAndTools.git-crypt
      cfg.gitPackage
      gnum4
      gnumake
      gnupg1orig # some scripts expect this version
      # gnupg22
      htop
      httpie
      iftop
      inotify-tools
      iotop
      ipcalc
      iptables
      jq
      libosinfo # osinfo-query
      libxml2 # xmllint
      lsof
      man-pages
      mc
      mosh
      nethogs
      nmap
      nox
      openjdk8
      openssl
      p7zip
      parallel
      pciutils
      psmisc
      pv
      ripgrep
      rpm
      rtorrent
      rxvt_unicode-with-plugins # I need it everywhere for terminfo entries. better way is to install only minimal rxvt on servers, but I don't care enough about space savings.
      screen
      socat
      sudo
      sysstat
      tcpdump
      inetutils
      tiptop
      unrar
      unzip
      usbutils
      vim
      wget
      which
      whois
      wol
      zip
      zsh
      nix-output-monitor
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
