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
      gnupg22
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
      manpages
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
      (platinum-searcher.overrideAttrs (old: { name = "lol"; force = "rebuild"; })) # binary cache for 'pt' is broken
      psmisc
      pv
      ripgrep
      rpm
      rtorrent
      rxvt_unicode-with-plugins # I need it everywhere for terminfo entries. better way is to install only minimal rxvt on servers, but I don't care enough about space savings.
      screen
      socat
      sox
      sudo
      sysstat
      tcpdump
      telnet
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
