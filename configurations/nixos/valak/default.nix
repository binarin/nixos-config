{ flake, config, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    flake.inputs.self.nixosModules.user-binarin
    ./configuration.nix
  ];

  inventoryHostName = "valak";

  hostConfig.features = [
    "vfio"
    "hyprland"
    "workstation"
    "lnxlink"
    "nix-builder"
    "move-xdg"
    "emacs"
  ];

  system.stateVersion = "24.11";
}
