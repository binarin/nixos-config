# This is your nixos configuration.
# For home configuration, see /modules/home/*
{ flake, pkgs, lib, config, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostOptions;
in
{
  options = {
    hostOptions = {
      gui.enable = lib.mkEnableOption "Whether to enable gui-related things on this host";
      managedUsers = lib.mkOption {
        default = [ ];
        type = lib.types.listOf lib.types.str;
        description = ''
          Users that should be instantiated on this host
        '';
      };
    };
  };

  config = {
    # These users can add Nix caches.
    nix.settings.trusted-users = [ "root" ] ++ cfg.managedUsers;

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = lib.mkForce false;

    nixpkgs.config.allowUnfree = true;

    home-manager.users = lib.genAttrs cfg.managedUsers (user:
      {
        imports = [
          self.homeModules.default
          { gui.enable = cfg.gui.enable; }
          (self + "/configurations/home/" + user + ".nix")
        ];
      }
    );

    services.openssh.enable = true;
  };
}
