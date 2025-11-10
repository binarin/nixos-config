{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixosSharedModules = [ self.nixosModules.home-manager ];

  flake.nixosModules.home-manager =
    {
      config,
      lib,
      specialArgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.home-manager";

      imports = [ inputs.home-manager.nixosModules.home-manager ];

      config = {
        home-manager.sharedModules = [
          self.homeModules.home-misc
        ];
        home-manager.extraSpecialArgs = specialArgs;
        home-manager.useGlobalPkgs = true;
        home-manager.backupFileExtension = "backup";

        # So for small changes I can run only home-manager activation script, to reduce iteration time
        home-manager.useUserPackages = lib.mkForce false;
      };
    };

  flake.homeModules.home-misc =
    { lib, ... }:
    {
      key = "nixos-config.modules.home.misc";

      config = {
        home.keyboard = lib.mkDefault null;
        home.activation = {
          removeCommonConfictingFiles = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
            $DRY_RUN_CMD rm -fv ~/.gtkrc-2.0 ~/.gtkrc-2.0.backup
          '';
        };
      };
    };
}
