{ ... }:
{
  flake.nixosModules.bluetooth =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.bluetooth";
      config = {
        environment.systemPackages = with pkgs; [
          bluetui
        ];

        services.blueman.enable = true;

        hardware.bluetooth = {
          enable = true;
          powerOnBoot = true;
        };

        systemd.tmpfiles.settings."10-bluetooth-persist" = lib.mkIf config.impermanence.enable {
          "/persist/var/lib/bluetooth".d = {
            user = "root";
            group = "root";
            mode = "0700";
          };
        };

        systemd.services.bluetooth.serviceConfig.BindPaths = lib.mkIf config.impermanence.enable [
          "/persist/var/lib/bluetooth:/var/lib/bluetooth"
        ];
      };
    };
}
