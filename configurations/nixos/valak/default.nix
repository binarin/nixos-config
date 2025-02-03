{ flake, config, ... }:
{
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];

  inventoryHostName = "valak";

  hostConfig.managedUsers = [ "binarin" ];

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
