{
  inputs,
  lib,
  ...
}:
let
  nixpkgsConfig = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
    permittedInsecurePackages = [
      # used by trezor-agent, but vulnerability is about leaking
      # generated keys - so doesn't matter, as keys do not leave
      # trezor
      "python3.13-ecdsa-0.19.1"
    ];
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
      imports = [
        inputs.determinate.nixosModules.default
      ];

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
