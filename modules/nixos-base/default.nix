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
      modulesPath,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.nixos-base";

      # clan.core.enableRecommendedDefaults is true by default and adds
      # gitMinimal + dnsutils + tcpdump + curl + jq + htop + nixos-facter to
      # every machine's system closure (~250 MiB). A bare base does not want
      # those debugging tools forced in.
      clan.core.enableRecommendedDefaults = lib.mkDefault false;

      # nixos-base targets machines that are deployed to (via clan/deploy-rs),
      # not machines that rebuild themselves. Drop nixos-rebuild,
      # nixos-generate-config, nixos-install, nixos-option etc. from the system
      # closure (~870 MiB combined). These are gated on
      # `config.nix.enable && !config.system.disableInstallerTools`.
      system.disableInstallerTools = lib.mkDefault true;

      # Keep all the perlless activation-modernization (initrd systemd,
      # etc-overlay, userborn) but drop the hard `forbiddenDependenciesRegexes
      # = [ "perl" ]` assertion. The modernized activation means perl is no
      # longer pulled in by default, so we keep the size win without making the
      # build fail if a machine legitimately needs a perl-based tool.
      system.forbiddenDependenciesRegexes = lib.mkForce [ ];

      imports = [
        # Bootable nixpkgs shrink profiles:
        #  - minimal: docs off, defaultPackages [], stub-ld off, xdg off, ...
        #  - perlless: initrd systemd + etc-overlay + userborn (replaces
        #    perl-based activation).
        # Together these get a real bootable system close to the nixpkgs floor
        # without the nuclear stubbing used for non-NixOS sidecars.
        (modulesPath + "/profiles/minimal.nix")
        (modulesPath + "/profiles/perlless.nix")

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
