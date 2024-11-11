# This is your nixos configuration.
# For home configuration, see /modules/home/*
{ flake, pkgs, lib, config, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  imports = [
    self.sharedModules.default
    self.nixosModules.bleeding
    self.nixosModules.fonts
    self.nixosModules.sops
  ];

  options = { };

  config = {
    networking.hostName = config.inventoryHostName;

    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes ca-derivations
      '';
    };

    # These users can add Nix caches.
    nix.settings.trusted-users = [ "root" ] ++ cfg.managedUsers;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    home-manager.useGlobalPkgs = true;

    # So for small changes I can run only home-manager activation script, to reduce iteration time
    home-manager.useUserPackages = lib.mkForce false;

    nixpkgs.config.allowUnfree = true;

    home-manager.users = lib.genAttrs cfg.managedUsers (user:
      {
        imports = [
          self.homeModules.default
          (self + "/configurations/home/" + user + ".nix")
        ];
        config = {
          inherit (config) hostConfig inventoryHostName;
          home.homeDirectory = config.users.users."${user}".home;
          home.username = user;
        };
      }
    );

    services.openssh.enable = true;

    # tailscale can't use exit nodes otherwise
    # citing https://github.com/tailscale/tailscale/issues/4432#issuecomment-1112819111:
    #  Blindly applying strict RPF to all traffic just doesn't work
    #  any more in modern network environments, especially desktops
    #  which usually have multiple active interfaces at once and
    #  complex things like Tailscale going on.
    networking.firewall.checkReversePath = "loose";
  };
}
