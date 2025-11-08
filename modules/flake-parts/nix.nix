{ inputs, ... }:
let
  nixpkgsConfig = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };
in
{

  perSystem =
    { pkgs, system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = nixpkgsConfig;
      };
    };

  flake.nixosModules.nix =
    { ... }:
    {
      config = {
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
