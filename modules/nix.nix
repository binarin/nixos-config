{
  inputs,
  lib,
  ...
}:
let
  nixpkgsConfig = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };
in
{
  flake-file.inputs = {
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
  };

  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = nixpkgsConfig;
      };
    };

  flake.nixosModules.nix =
    { ... }:
    {
      key = "nixos-config.modules.nixos.nix";
      config = {
        nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

        nix = {
          settings = {
            sandbox = true;
            substituters = [ "https://cache.nixos.org" ];
          };
          extraOptions = ''
            experimental-features = nix-command flakes ca-derivations
          '';
        };

        nix.settings.trusted-users = [ "root" ];

        nixpkgs.config = nixpkgsConfig;
      };
    };

}
