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
    flake.inputs.self.nixosModules.vscode-remote-workaround
  ];
  config = lib.mkIf config.hostConfig.feature.wsl {
    wsl.defaultUser = lib.mkDefault (lib.elemAt (config.hostConfig.managedUsers) 0);

    # XXX remove on 24.11
    nixpkgs.overlays = [
      (final: prev: { switch-to-configuration-ng = final.bleeding.switch-to-configuration-ng; })
    ];

    # XXX Guard on 'gui'
    environment.variables.LD_LIBRARY_PATH = "/run/opengl-driver/lib/";
    wsl.useWindowsDriver = true;
    hardware.opengl.enable = true;
    hardware.opengl.driSupport = true;

    wsl.enable = true;
    wsl.startMenuLaunchers = true;
    wsl.usbip.enable = true;
    wsl.wslConf.network.generateHosts = false;
    environment.systemPackages = with pkgs; [ wslu ];
    vscode-remote-workaround.enable = true;
  };
}
