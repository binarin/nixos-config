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
    {
      pkgs,
      system,
      self',
      ...
    }:
    let
      ssh-ca = inputs.common.packages.${system}.ssh-ca;
    in
    {
      legacyPackages.ssh-trust-manifest = pkgs.writeText "ssh-trust-manifest.json" (
        builtins.toJSON (inputs.common.lib.sshTrust.mkManifest entries)
      );

      # `nix run .#ssh-ca -- session|sign ...` against this flake's hosts.
      legacyPackages.ssh-ca-fleet = pkgs.writeShellApplication {
        name = "ssh-ca-fleet";
        text = ''
          if [ "$#" -eq 0 ]; then
            exec ${lib.getExe ssh-ca} --help
          fi
          command="$1"
          shift
          exec ${lib.getExe ssh-ca} "$command" --policy ${self'.legacyPackages.ssh-trust-manifest} "$@"
        '';
      };
      apps.ssh-ca = {
        type = "app";
        meta.description = "ssh-ca with this flake's target manifest";
        program = lib.getExe self'.legacyPackages.ssh-ca-fleet;
      };
    };
}
