{ self, ... }:
{
  nixosSharedModules = [ self.nixosModules.standard-linux-tools ];

  flake.nixosModules.standard-linux-tools =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.standard-linux-tools";

      config = lib.mkIf config.hostConfig.feature.interactive-cli or false {

        programs.iftop.enable = true;

        programs.iotop.enable = true;

        programs.mosh.enable = true;

        programs.wireshark.enable = true;
        programs.wireshark.package =
          if config.hostConfig.feature.gui or false then pkgs.wireshark-qt else pkgs.tshark;

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
          enable = config.hostConfig.feature.interactive-cli or false;
        };
      };
    };
}
