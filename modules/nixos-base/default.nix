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

  # Provisions SSH authorized keys for `root` on every machine via clan's
  # built-in `sshd` distributed-service module (its server role only ever
  # writes `users.users.root.openssh.authorizedKeys.keys` — never a normal
  # user; `binarin` is granted access in the nixos-base module below). Keys
  # come from the secure (hardware-backed) entries in inventory/public-keys.nix.
  clan.inventory.instances.binarin-admin = {
    module.name = "sshd";
    roles.server.tags.all = { };
    roles.server.settings = {
      authorizedKeys =
        with lib;
        pipe (import "${self}/inventory/public-keys.nix").ssh_keys [
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

      # enableRecommendedDefaults bundles the debug-package bloat (gitMinimal,
      # tcpdump, ...) together with clan's automatic stateVersion generator.
      # We disabled the whole bundle above, so re-enable just the stateVersion
      # generator (it picks up system.stateVersion from the nixos release or
      # from a clan var) to avoid the "system.stateVersion is not set" warning.
      clan.core.settings.state-version.enable = lib.mkDefault true;

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

      # nixpkgs.flake.setFlakeRegistry is on by default for flake-built
      # systems and pins /etc/nix/registry.json to the full nixpkgs source tree
      # (~206 MiB closure entry). It exists so `nix run nixpkgs#hello` reuses
      # system deps — irrelevant on deployed-only machines, so drop it (and
      # setNixPath, which asserts on setFlakeRegistry).
      nixpkgs.flake.setFlakeRegistry = lib.mkDefault false;
      nixpkgs.flake.setNixPath = lib.mkDefault false;

      # The `binarin` user comes from the clan `users` service instance above,
      # but clan's `sshd` service grants *root* only, and modules/sshd.nix
      # likewise covers root only. Without this, binarin exists on every
      # nixos-base machine yet has neither an authorized_keys.d entry nor an
      # authorized_principals.d entry, so sshd rejects it with "Certificate
      # does not contain an authorized principal".
      #
      # `authorizedKeys.keys` must NOT be mkDefault here: clan's `users`
      # service assigns `openssh.authorizedKeys` at normal priority (with an
      # empty `keys` default), which would outrank a mkDefault and leave the
      # list empty. At equal priority the two list definitions concatenate.
      # `authorizedPrincipals` is untouched by clan, so mkDefault is fine there
      # and lets the fuller binarin-baseline definition win if a machine ever
      # imports both.
      users.users.binarin.openssh = {
        authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "binarin";
        authorizedPrincipals = lib.mkDefault [
          "root"
          "binarin"
        ];
      };

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
