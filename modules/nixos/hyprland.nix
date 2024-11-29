# -*- nix -*-
{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  hyprlandSessionScript = pkgs.writeShellApplication {
    name = "hyprland-uwsm-custom-start";
    runtimeInputs = with pkgs; [
      coreutils
      bash
      systemd
      dbus
    ];

    text = ''
      dbus-update-activation-environment --systemd PAM_KWALLET5_LOGIN
      ${lib.getExe pkgs.uwsm} start -S -F ${lib.getExe config.programs.hyprland.package}
    '';
  };

  hyprlandSession = pkgs.writeTextFile {
    name = "hyprland-uwsm-custom";
      text = ''
        [Desktop Entry]
        Name=Hyprland (UWSM/Custom)
        Comment=kwallet-compatible Hyprland(UWSM) session
        Exec=${lib.getExe hyprlandSessionScript}
        Type=Application
      '';
      destination = "/share/wayland-sessions/hyprland-uwsm-custom.desktop";
      derivationArgs = {
        passthru.providedSessions = [ "hyprland-uwsm-custom" ];
      };
    };

in
{
  config = lib.mkIf config.hostConfig.feature.hyprland {

    nixpkgs.overlays = [
      inputs.hyprland-contrib.overlays.default
      inputs.hyprland.overlays.default
    ];

    services.displayManager.sddm.enable = true;
    services.displayManager.defaultSession = "hyprland-uwsm-custom";
    services.displayManager = {
      sessionPackages = [ hyprlandSession ];
    };

    security.pam.services.login.kwallet.enable = true;
    security.pam.services.login.kwallet.package = lib.mkForce pkgs.kdePackages.kwallet-pam;

    programs.hyprlock.enable = true;
    programs.hyprland.enable = true;
    programs.hyprland.withUWSM = true;
  };
}
