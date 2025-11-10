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
      config = {
        environment.systemPackages = with pkgs; [
          bluetui
        ];

        services.blueman.enable = true;

        hardware.bluetooth = {
          enable = true;
          powerOnBoot = true;
        };

        systemd.services.bluetooth.serviceConfig.BindPaths = lib.mkIf config.impermanence.enable [
          "/local/var/lib/bluetooth:/var/lib/bluetooth"
        ];
      };
    };
}
