{ self, inputs, ... }:
{
  flake-file.inputs = {
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  flake.nixosModules.microsoft-surface =
    { config, ... }:
    {
      key = "nixos-config.nixos.microsoft-surface";
      imports = [
        inputs.nixos-hardware.nixosModules.microsoft-surface-pro-intel
        self.modules.generic.flake-files
      ];

      hardware.microsoft-surface.kernelVersion = "stable";
      # XXX broken after rust was bumped
      boot.kernelPatches = [
        {
          name = "rust-1.91-fix";
          patch = config.lib.self.file "rust-fix.patch";
        }
      ];
      boot.initrd.kernelModules = [
        "surface_aggregator"
        "surface_aggregator_registry"
        "surface_aggregator_hub"
        "surface_aggregator_tabletsw"
        "surface_hid_core"
        "surface_hid"
        "8250_dw"
        "intel_lpss_pci"
        "intel_lpss"
        "pinctrl_tigerlake"
      ];
    };
}
