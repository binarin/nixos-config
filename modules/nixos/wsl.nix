{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    flake.inputs.nixos-wsl.nixosModules.default
  ];
  config = lib.mkIf config.hostConfig.feature.wsl {
    wsl.defaultUser = "binarin";

    # XXX Guard on 'gui'
    environment.variables.LD_LIBRARY_PATH = "/run/opengl-driver/lib/";
    wsl.useWindowsDriver = true;
    hardware.graphics.enable = true;

    wsl.enable = true;
    wsl.startMenuLaunchers = true;
    wsl.usbip.enable = true;
    wsl.wslConf.network.generateHosts = false;
    environment.systemPackages = with pkgs; [ wslu ];
    vscode-remote-workaround.enable = true;
  };
}
