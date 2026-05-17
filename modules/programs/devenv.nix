{
  self,
  inputs,
  lib,
  ...
}:
{
  flake.nixosModules.devenv =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.devenv";
      environment.systemPackages = with pkgs; [
        bleeding.devenv
      ];
    };

  flake.homeModules.devenv =
    {
      pkgs,
      lib,
      osConfig ? null,
      ...
    }:
    {
      key = "nixos-config.modules.home.devenv";
      config = lib.mkIf (osConfig == null) {
        home.packages = [
          pkgs.bleeding.devenv
        ];
      };
    };

}
