{
  self,
  config,
  lib,
  inputs,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.nixosModules.metabase-db-generator =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.metabase-db-generator";
      config.clan.core.vars.generators.metabase-db = {
        share = true;
        files.password = {
          secret = true;
          deploy = true;
          restartUnits = [ "metabase-db-password.service" ];
        };
        runtimeInputs = [ pkgs.openssl ];
        script = ''
          openssl rand -hex 32 > $out/password
        '';
      };
    };
}
