{flake, lib, pkgs, config, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.nixosModules.default
    ./configuration.nix
  ];

  config = {
    inventoryHostName = "iso";

    hostConfig.managedUsers = [ "root" "nixos" ];
    hostConfig.features = [
      "interactive-cli"
      "nix-builder"
    ];

    nixpkgs.hostPlatform = "x86_64-linux";

    # NOTE: Don't bump without re-installing complete system!
    system.stateVersion = "24.11";
  };
}
