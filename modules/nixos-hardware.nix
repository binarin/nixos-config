{ self, inputs, ... }:
let
  selfLib = self.lib.self;
in
{
  flake-file.inputs = {
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  flake.nixosModules.microsoft-surface =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.microsoft-surface";
      imports = [
        inputs.nixos-hardware.nixosModules.microsoft-surface-pro-intel
      ];

      hardware.microsoft-surface.kernelVersion = "stable";
    };
}
