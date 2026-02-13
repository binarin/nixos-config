# modules/machines/yolo-nixos-config.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.yolo-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "yolo-nixos-config";
    };
    modules = [
      self.nixosModules.yolo-nixos-config-configuration
    ];
  };

  flake.nixosModules.yolo-nixos-config-configuration =
    { modulesPath, ... }:
    {
      key = "nixos-config.modules.nixos.yolo-nixos-config-configuration";
      imports = [
        (modulesPath + "/profiles/qemu-guest.nix")
        self.nixosModules.default
      ];

      config = {
        nixos-config.export-metrics.enable = false;
        networking.hostName = "yolo-nixos-config";
        system.stateVersion = "25.11";

        boot.loader.grub.device = "/dev/vda";
        fileSystems."/" = {
          device = "/dev/vda1";
          fsType = "ext4";
        };
      };
    };
}
