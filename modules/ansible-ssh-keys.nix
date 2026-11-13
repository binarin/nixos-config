{ inputs, lib, ... }:
let
  sshTrust = inputs.common.lib.sshTrust;
  grants = import ../inventory/ansible-ssh-grants.nix;

  # Projection of the common registry into the shape the Ansible sshd role
  # templates expect: `presence` marks direct-login keys, `user-ca` marks
  # trusted user CAs, `force_install_on` carries per-host grants.
  ansibleSshKeys = {
    ssh_keys = lib.mapAttrs (
      id: key:
      {
        public_key = key.publicKey;
        inherit (key) description;
        tags = lib.optional key.trust.login "presence" ++ lib.optional key.trust.userCa "user-ca";
      }
      // lib.optionalAttrs (grants ? ${id}) { force_install_on = grants.${id}; }
    ) (lib.filterAttrs (id: key: key.trust.login || key.trust.userCa || grants ? ${id}) sshTrust.keys);
  };
in
{
  perSystem =
    { pkgs, ... }:
    {
      legacyPackages.ansible-ssh-public-keys = (pkgs.formats.yaml { }).generate "ssh-public-keys.yaml" ansibleSshKeys;
    };
}
