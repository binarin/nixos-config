{ flake, pkgs, config, lib, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.perl-packages
  ];
  config = {
    environment.systemPackages = with pkgs; [
      bat

      bind # for dig
      binutils
      bridge-utils
      broot
      cryptsetup
      curl
      darcs
      dpkg
      elinks
      file
      fzf
      gdb
      git
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
      ov
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
