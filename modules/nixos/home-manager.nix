{
  flake,
  pkgs,
  lib,
  config,
  specialArgs,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in {
  imports = [inputs.home-manager.nixosModules.home-manager];

  config = {
    home-manager.extraSpecialArgs = specialArgs;
    home-manager.useGlobalPkgs = true;
    home-manager.backupFileExtension = "backup";

    # So for small changes I can run only home-manager activation script, to reduce iteration time
    home-manager.useUserPackages = lib.mkForce false;

    home-manager.users = lib.genAttrs cfg.managedUsers (user: {
      imports = [
        self.homeModules.default
        (self + "/configurations/home/" + user + ".nix")
      ];
      config = {
        inherit (config) hostConfig inventoryHostName;
        home.homeDirectory = config.users.users."${user}".home;
        home.username = user;
      };
    });
  };
}
