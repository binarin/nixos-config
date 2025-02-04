{ flake, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in {
  imports = [
    flake.inputs.self.nixosModules.default
    ./configuration.nix
  ];


  inventoryHostName = "demandred";

  hostConfig.managedUsers = [ "root" "binarin" ];

  hostConfig.features = [
    "hyprland"
    "interactive-cli"
    "impermanence"
    "emacs"
    "workstation"
    # "airgapped"
    "nix-builder"
  ];
  system.stateVersion = "24.11";
}
