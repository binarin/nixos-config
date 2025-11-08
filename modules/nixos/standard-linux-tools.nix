{
  pkgs,
  config,
  lib,
  ...
}:
let
  defEnable = config.hostConfig.lib.defaults.enable;
in
{
  config = lib.mkIf config.hostConfig.feature.interactive-cli {

    programs.iftop.enable = defEnable;

    programs.iotop.enable = defEnable;

    programs.mosh.enable = defEnable;

    programs.wireshark.enable = defEnable;
    programs.wireshark.package =
      if config.hostConfig.feature.gui then pkgs.wireshark-qt else pkgs.tshark;

    environment.systemPackages = with pkgs; [
      bridge-utils
      cryptsetup
      inotify-tools
      iptables
      nftables
      pciutils
      usbutils
    ];

    environment.enableAllTerminfo = true;

    services.locate = {
      enable = config.hostConfig.feature.interactive-cli;
    };
  };
}
