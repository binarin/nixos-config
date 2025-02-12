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
      homeDirectory = self.helpers.user-dirs.homeDir user;
    in {config, osConfig, ...}: {
      imports = [
        self.homeModules.default
      ] ++ (lib.optional (builtins.pathExists hmConfigurationFile) hmConfigurationFile);
      config = {
        inherit (osConfig) hostConfig inventoryHostName;

        # XXX I don't know yet why it can easily cause infinite recursion
        home.homeDirectory = homeDirectory;

        xdg.cacheHome = "${homeDirectory}/${config.xdg.cacheHomeRelative}";
        xdg.configHome = "${homeDirectory}/${config.xdg.configHomeRelative}";
        xdg.dataHome = "${homeDirectory}/${config.xdg.dataHomeRelative}";
        xdg.stateHome = "${homeDirectory}/${config.xdg.stateHomeRelative}";

        home.username = user;
        home.stateVersion = osConfig.system.stateVersion;
      };
    });
  };
}
