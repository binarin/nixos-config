{inputs, ...}: {
  flake-file.inputs = {
    bluetui.url = "github:pythops/bluetui";
    bluetui.inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.nixosModules.bluetooth = {config, lib, pkgs, ...}: {
    config = {
      environment.systemPackages = [
        inputs.bluetui.packages."${pkgs.stdenv.system}".bluetui
      ];

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
