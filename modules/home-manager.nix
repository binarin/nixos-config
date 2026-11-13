{
  self,
  inputs,
  ...
}:
{
  imports = [
    inputs.home-manager.flakeModules.home-manager
  ];

  flake.nixosModules.home-manager =
    {
      specialArgs,
      self',
      inputs',
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.home-manager";

      imports = [ inputs.home-manager.nixosModules.home-manager ];

      config = {
        home-manager.sharedModules = [
          inputs.common.homeModules.local-checkout-links
          self.homeModules.home-misc
        ];
        home-manager.extraSpecialArgs = specialArgs // {
          inherit self' inputs';
        };
        home-manager.useGlobalPkgs = true;
        home-manager.backupCommand = lib.getExe pkgs.trash-cli;
      };
    };

  flake.homeModules.home-misc =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.home.home-misc";

      config = {
        home.localCheckoutLinks.checkouts.nixos-config = lib.mkDefault "${config.home.homeDirectory}/personal-workspace/swan-lake-umbrella/nixos-config";
        news.display = "silent";
        xdg.userDirs.setSessionVariables = false; # 26.05 default
        home.keyboard = lib.mkDefault null;
        home.activation = {
          removeCommonConfictingFiles = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
            $DRY_RUN_CMD rm -fv ~/.gtkrc-2.0 ~/.gtkrc-2.0.backup
            $DRY_RUN_CMD rm -fv ~/.config/emacs/{early-,}init.el{,c}
          '';
        };
      };
    };
}
