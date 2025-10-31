{flake, pkgs, lib, config, specialArgs, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];

  options.home-manager.defaultImports = lib.mkOption {
    type = lib.types.listOf lib.types.deferredModule;
  };

  config = {
    home-manager.defaultImports = [ self.homeModules.default ];
    home-manager.extraSpecialArgs = specialArgs;
    home-manager.useGlobalPkgs = true;
    home-manager.backupFileExtension = "backup";

    # So for small changes I can run only home-manager activation script, to reduce iteration time
    home-manager.useUserPackages = lib.mkForce false;

    home-manager.users = lib.genAttrs cfg.managedUsers (user: let
      hmConfigurationFile = self + "/configurations/home/" + user + ".nix";
      homeDirectory = self.helpers.user-dirs.homeDir user;
    in {config, osConfig, ...}: {
      imports = osConfig.home-manager.defaultImports ++ lib.optional (builtins.pathExists hmConfigurationFile) hmConfigurationFile;
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
