{
  flake,
  config,
  pkgs,
  lib,
  bleeding,
  stdenv,
  ...
}:
let
in
{
  boot = {
    supportedFilesystems = [
      "exfat"
      "nfs"
      "cifs"
    ];
    kernelModules = [
      "fuse"
      "v4l2loopback"
    ];
    extraModprobeConfig = ''
      options v4l2loopback video_nr=7,8      options v4l2loopback card_label=Canon,OBS
    '';
    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
    kernel.sysctl."vm.swappiness" = 1;
  };

  services.resolved = {
    enable = true;
  };

  networking = {
    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
    resolvconf.dnsExtensionMechanism = false;
    extraHosts = ''
            127.0.0.1 ${config.networking.hostName}
      #      0.0.0.0 pikabu.ru leprosorium.ru shovinist.leprosorim.ru idiod.leprosorium.ru games.leprosorium.ru meduza.io d3.ru
    '';
    nat = {
      enable = true;
      internalInterfaces = [ "virbr0" ];
      externalInterface = "wlp2s0";
    };
  };

  i18n = {
    defaultLocale = "nl_NL.UTF-8";
    extraLocaleSettings = {
      LC_NUMERIC = "en_US.UTF-8";
      LC_TIME = "C.UTF-8";
    };
    supportedLocales = [ "all" ];
  };

  time.timeZone = "Europe/Amsterdam";

  programs.nix-ld.enable = true;
  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark-qt;

  # programs.chromium.enable = true;
  # programs.chromium.extraOpts = {
  #   ExternalProtocolDialogShowAlwaysOpenCheckbox = true;
  # };

  # environment.variables = {
  #   GDK_SCALE = "2";
  #   GDK_DPI_SCALE = "0.5";
  #   _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  # };

  environment.systemPackages =
    let
      nixDevPackages = with pkgs; [ patchelf ];
      utilityPackages = with pkgs; [
        pbzip2
        pigz
        ntfs3g
        gopass
        jekyll
        pdftk
        # syncthing
        (texlive.combine { inherit (texlive) scheme-full beamer ps2eps; })
        lilypond-with-fonts
        abcm2ps
        # vagrant
        virt-viewer
        virt-manager
        borgbackup
      ];
      otherPackages = with pkgs; [
        yt-dlp
        gnuplot
        dconf
        dconf-editor
        gnome-tweaks
        dmenu
        dmidecode
        gmrun
        # haskellPackages.xmobar
        # haskellPackages.yeganesh
        hsetroot
        quasselClient
        keychain
        libreoffice
        mu
        pavucontrol
        wmctrl
        xclip
        xscreensaver
        xsel
      ];
    in
    otherPackages ++ nixDevPackages ++ utilityPackages;

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };




  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      rocmPackages.clr
      intel-media-driver
    ];
  };

  hardware.flipperzero.enable = true;

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";
  services.openssh.settings.X11Forwarding = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  services.trezord.enable = true;

  services.cron.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [
    pkgs.hplip
    pkgs.postscript-lexmark
    pkgs.epson-escpr
  ];

  programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
  services.xserver = {

    modules = [ pkgs.xorg.xf86inputlibinput ];
    videoDrivers = [
      "amdgpu"
      "modesetting"
    ];
    config = ''
      Section "Device"
           Identifier "AMD"
           Driver "amdgpu"
           Option "VariableRefresh" "true"
           Option "TearFree" "on"
      EndSection

      Section "InputClass"
      	Identifier "CirqueTouchpad1"
      	MatchProduct "GlidePoint"
      	Option "SwapAxes" "True"
      	Option "Emulate3Buttons" "True"
      	Option "InvertY" "True"
        Option "Tapping" "False"
        Option "TappingDrag" "False"
      EndSection

      Section "InputClass"
      	Identifier "CirqueTouchpad2"
        MatchUSBID "0488:0280"
        Option "TransformationMatrix" "0 1 0 -1 0 1 0 0 1"
        Option "MiddleEmulation" "True"
      	Option "Emulate3Buttons" "True"
        Option "Tapping" "False"
        Option "TappingDrag" "False"
      EndSection

      Section "InputClass"
        Identifier "Yubikey"
        MatchUSBID "1050:0403"
        Option "XkbModel" "pc104"
        Option "XkbLayout" "us"
        Option "XkbOptions" ""
        Option "XkbVariant" ""
      EndSection

      Section "InputClass"
          Identifier      "Marble Mouse"
          MatchUSBID      "046d:c408"
          Driver          "libinput"
          Option "ScrollMethod" "button"
          Option "ScrollButton" "8"
          Option "MiddleEmulation" "on"
      EndSection

      Section "Device"
              Identifier  "Intel Graphics"
              Driver      "intel"
              Option      "TearFree" "true"
      EndSection
    '';
    enable = true;
    xkb.layout = "us,ru";

    # xkbDir = "${pkgs.xorg.xkeyboardconfig_dvp}/share/X11/xkb";
    xkb.variant = ",winkeys";
    xkb.options = "grp:menu_toggle,ctrl:nocaps,altwin:super_win,grp:sclk_toggle";

    # libinput = {
    #   enable = lib.mkForce false;
    #   clickMethod = "none";
    #   middleEmulation = true;
    #   tapping = false;
    # };

    desktopManager.xterm.enable = true;
    desktopManager.gnome.enable = false;

    displayManager = {
      gdm.enable = false;
      lightdm.enable = false;
    };
  };

  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.enable = true;

  location.latitude = 52.3702;
  location.longitude = 4.8952;

  users.extraUsers = {
    root = {
      shell = "/run/current-system/sw/bin/zsh";
    };
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    27036
    27037
  ];
  networking.firewall.allowedUDPPorts = [
    27031
    27036
  ];

  services.tailscale.enable = true;

  virtualisation = {
    docker.enable = true;
    docker.storageDriver = "overlay2";
    libvirtd.enable = true;
  };

  zramSwap = {
    enable = true;
  };

  programs.java = {
    enable = true;
    package = pkgs.openjdk11;
  };

  programs.ssh.startAgent = true;
  programs.light.enable = true;

  systemd.services."binarin-org-sync" =
    let
      script = pkgs.writeScript "binarin-org-sync" ''
        #!${pkgs.bash}/bin/bash
        set -euo pipefail
        if [[ -f /home/binarin/org/push.sh ]]; then
          exec /home/binarin/org/push.sh
        fi
      '';
    in
    {
      description = "Syncs org-mode files via git";
      path = [
        pkgs.gitAndTools.gitFull
        pkgs.wget
        pkgs.coreutils
        pkgs.bash
      ];
      serviceConfig = {
        Type = "oneshot";
        User = "binarin";
        ExecStart = script;
      };
    };

  systemd.timers."binarin-org-sync" = {
    description = "Periodically updates org-mode files via git";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "15min";
    };
  };

  # TLP brings you the benefits of advanced power management for Linux without the need to understand every technical detail.
  # services.tlp.enable = true;

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
  };

  services.udev.extraRules = ''
    ACTION=="add", \
    ATTR{idVendor}=="04a9", \
    ATTR{idProduct}=="3218", \
    ENV{SYSTEMD_WANTS}+="external_webcam.service", \
    SUBSYSTEM=="usb", \
    TAG+="systemd"

    SUBSYSTEM=="video4linux", \
    KERNEL=="video[0-9]*", \
    ACTION=="add", \
    ENV{ID_VENDOR_ID}=="046d", \
    ENV{ID_MODEL_ID}=="0892", \
    RUN+="${pkgs.v4l-utils}/bin/v4l2-ctl --set-ctrl zoom_absolute=180,pan_absolute=10800 -d %N"

    KERNEL=="hidraw*", \
    SUBSYSTEM=="hidraw", \
    ATTRS{idVendor}=="19f5", \
    ATTRS{idProduct}=="3255", \
    MODE="0660", \
    GROUP="users", \
    TAG+="uaccess", \
    TAG+="udev-acl"
  '';

  systemd.services.external_webcam =
    let
      hosts = import ../nixops/personal-hosts.nix;
      lightsOn = pkgs.writeShellScript "elgatos-on" ''
        ${pkgs.curl}/bin/curl -X PUT -H "Content-Type: application/json"  --json '{"numberOfLights":1,"lights":[{"on":1,"brightness":47,"temperature":213}]}' http://${hosts.elgato-key-left.lan.ip}:9123/elgato/lights || true
        ${pkgs.curl}/bin/curl -X PUT -H "Content-Type: application/json"  --json '{"numberOfLights":1,"lights":[{"on":1,"brightness":47,"temperature":213}]}' http://${hosts.elgato-key-right.lan.ip}:9123/elgato/lights || true
      '';
      lightsOff = pkgs.writeShellScript "elgatos-off" ''
        ${pkgs.curl}/bin/curl -X PUT -H "Content-Type: application/json"  --json '{"numberOfLights":1,"lights":[{"on":0,"brightness":47,"temperature":213}]}' http://${hosts.elgato-key-left.lan.ip}:9123/elgato/lights || true
        ${pkgs.curl}/bin/curl -X PUT -H "Content-Type: application/json"  --json '{"numberOfLights":1,"lights":[{"on":0,"brightness":47,"temperature":213}]}' http://${hosts.elgato-key-right.lan.ip}:9123/elgato/lights || true
      '';
    in
    {
      enable = true;
      script = ''
        ${pkgs.gphoto2}/bin/gphoto2 --stdout --capture-movie |
        ${pkgs.ffmpeg}/bin/ffmpeg -i - -vcodec rawvideo -pix_fmt yuv420p -f v4l2  /dev/video7
      '';
      serviceConfig = {
        ExecStopPost = "${lightsOff}";
        ExecStartPost = "${lightsOn}";
      };
      # wantedBy = ["multi-user.target"];
    };

  services.gnome.gnome-keyring.enable = lib.mkForce false;
  services.flatpak.enable = true;
  services.joycond.enable = true;

  programs.adb.enable = true;

  services.pcscd.enable = true;
}
