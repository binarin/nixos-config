{flake, pkgs, lib, config, specialArgs, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];

  config = {
    home-manager.sharedModules = [ self.homeModules.default ];
    home-manager.extraSpecialArgs = specialArgs;
    home-manager.useGlobalPkgs = true;
    home-manager.backupFileExtension = "backup";

    # So for small changes I can run only home-manager activation script, to reduce iteration time
    home-manager.useUserPackages = lib.mkForce false;
  };
}
