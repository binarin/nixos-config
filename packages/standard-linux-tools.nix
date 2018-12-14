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
      wireshark-package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.wireshark-cli;
      };
    };
  };
  config = {
    environment.systemPackages = with pkgs; [
      apg
      bind # for dig
      binutils
      bridge-utils
      cryptsetup
      darcs
      dpkg
      elinks
      file
      fzf
      gdb
      gitAndTools.diff-so-fancy
      gitAndTools.git-annex
      gitAndTools.gitFull
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
      openssl
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
      cfg.wireshark-package
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
