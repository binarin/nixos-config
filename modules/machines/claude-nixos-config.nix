# modules/machines/claude-nixos-config.nix
{ self, inputs, ... }:
{
  flake.nixosConfigurations.claude-nixos-config = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inventoryHostName = "claude-nixos-config";
    };
    modules = [
      self.nixosModules.claude-nixos-config-configuration
    ];
  };

  flake.nixosModules.claude-nixos-config-configuration =
    { ... }:
    {
      key = "nixos-config.modules.nixos.claude-nixos-config-configuration";
      imports = [
        self.nixosModules.qemu-guest
        self.nixosModules.default
      ];

      config = {
        networking.hostName = "claude-nixos-config";
        system.stateVersion = "25.11";
        nixos-config.export-metrics.enable = false;

        nixos-config.qemu-guest.proxmox = {
          memory = 4096;
          cores = 2;
          disks = [
            {
              type = "virtio";
              size = "32G";
              storage = "local-lvm";
              index = 0;
              bootOrder = 1;
            }
          ];
          onboot = false;
          agent = true;
          description = "Minimal NixOS VM";
        };

        boot.loader.grub.device = "/dev/vda";
        fileSystems."/" = {
          device = "/dev/vda1";
          fsType = "ext4";
        };
      };
    };
}
