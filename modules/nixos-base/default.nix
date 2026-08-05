{
  self,
  config,
  lib,
  ...
}:
let
  flakeConfig = config;
  selfLib = self.lib.self;
in
{
  # ---------------------------------------------------------------------------
  # clan inventory instances
  # ---------------------------------------------------------------------------

  # Creates the `binarin` user (uid 1000) + group (gid 1000) and adds it to
  # `wheel`, via clan's built-in `users` distributed-service module. The user
  # itself has no shell customization or home-manager config in this base.
  clan.inventory.instances.binarin-user = {
    module.name = "users";
    roles.default.tags.all = { };
    roles.default.settings = {
      user = "binarin";
      groups = [ "wheel" ];
    };
  };

  # Provisions SSH authorized keys for `binarin` on every machine via clan's
  # built-in `sshd` distributed-service module. Keys come from the secure
  # (hardware-backed) entries in inventory/public-keys.nix.
  clan.inventory.instances.binarin-admin = {
    module.name = "sshd";
    roles.server.tags.all = { };
    roles.server.settings = {
      authorizedKeys =
        with lib;
        pipe (import "${self}/inventory/public-keys.nix") [
          (filterAttrs (
            _:
            {
              secure ? null,
              ...
            }:
            secure == true
          ))
          (mapAttrs (_: { public_key, ... }: public_key))
        ];
    };
  };

  # ---------------------------------------------------------------------------
  # nixos-base NixOS module
  # ---------------------------------------------------------------------------

  flake.nixosModules.nixos-base =
    {
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.nixos-base";

      imports = [
        # Clan is mandatory in nixos-base. We import the two minimal clan
        # pieces directly rather than `clan-baseline`, so the tailscale OAuth
        # generators are not pulled into the bare base.
        self.nixosModules.clan-hostId
        self.nixosModules.clan-hosts

        # openssh + root authorized keys + CA principals.
        self.nixosModules.sshd

        # hostId, /etc/hosts, ipAllocation options.
        self.nixosModules.inventory

        # config.lib.publicKeys helper used by sshd.
        self.nixosModules.public-keys
      ];
    };
}
