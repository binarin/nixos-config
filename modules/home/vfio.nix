{
  flake,
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf config.hostConfig.feature.vfio {
    home.packages = with pkgs; [ looking-glass-client ];

    home.file.".config/libvirt/libvirt.conf".text = ''
      uri_default = "qemu:///system"
    '';

    home.file.".config/looking-glass/client.ini".text = ''
      [app]
      shmFile=/dev/shm/looking-glass
      renderer=auto
      allowDMA=yes

      [win]
      title=looking-glass-client
      autoResize=yes
      keepAspect=yes
      dontUpscale=yes
      noScreensaver=yes
      quickSplash=yes
      borderless=no
      fullScreen=yes
      uiFont=pango:Iosevka
      uiSize=16
      maximize=no
      showFPS=no

      [egl]
      vsync=yes
      multisample=yes
      scale=2

      [wayland]
      warpSupport=yes
      fractionScale=no

      # [input]
      # escapeKey=70
      # grabKeyboardOnFocus=yes
      # releaseKeysOnFocusLoss=yes
      # autoCapture=yes
      # rawMouse=yes

      # [spice]
      # enable=yes
      # host=/dev/shm/win10-4game_spice
      # port=5900
      # clipboard=yes
      # clipboardToVM=yes
      # clipboardToLocal=yes
    '';
  };
}
