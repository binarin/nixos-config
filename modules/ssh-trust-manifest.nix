{
  self,
  inputs,
  lib,
  ...
}:
let
  enabled = c: c.config.swanLake.sshTrust.enable or false;
  entries = map (c: c.config.swanLake.sshTrust.manifestEntry) (
    lib.filter enabled (builtins.attrValues self.nixosConfigurations ++ builtins.attrValues self.systemConfigs)
  );
in
{
  # Target-policy manifest for the ssh-ca signing CLI: which certificate
  # principals each machine/account accepts.
  perSystem =
    { pkgs, ... }:
    {
      legacyPackages.ssh-trust-manifest = pkgs.writeText "ssh-trust-manifest.json" (
        builtins.toJSON (inputs.common.lib.sshTrust.mkManifest entries)
      );
    };
}
