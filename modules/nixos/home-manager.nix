{flake, pkgs, lib, config, specialArgs, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];

  config = {
    home-manager.extraSpecialArgs = specialArgs;
    home-manager.useGlobalPkgs = true;
    home-manager.backupFileExtension = "backup";

    # So for small changes I can run only home-manager activation script, to reduce iteration time
    home-manager.useUserPackages = lib.mkForce false;

    home-manager.users = lib.genAttrs cfg.managedUsers (user: let
      hmConfigurationFile = self + "/configurations/home/" + user + ".nix";
    in {
      imports = [
        self.homeModules.default
      ] ++ (lib.optional (builtins.pathExists hmConfigurationFile) hmConfigurationFile);
      config = {
        inherit (config) hostConfig inventoryHostName;
        home.homeDirectory = config.users.users."${user}".home;
        home.username = user;
        home.stateVersion = config.system.stateVersion;
      };
    });
  };
}
