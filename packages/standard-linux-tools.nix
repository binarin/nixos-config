{pkgs, config, lib, ...}:
let
  cfg = config.standard-linux-tools;
in
{
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
      pkgs.bleeding.fzf # fzf-share is only here yet
      gdb
      pkgs.bleeding.gitAndTools.diff-so-fancy # broken in 17.03 as of 2017-08-22
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
      iptables
      jq
      libosinfo # osinfo-query
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
      (platinum-searcher.overrideAttrs (old: { force = "rebuild"; })) # binary cache for 'pt' is broken
      psmisc
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
      tmux
      unrar
      unzip
      usbutils
      vim
      wget
      which
      whois
      cfg.wireshark-package
      zip
      zsh
    ];
  };
}
