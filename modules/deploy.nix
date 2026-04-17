{ inputs, lib, ... }:
let
  self = inputs.self;
  system = "x86_64-linux";
  unmodifiedPkgs = import inputs.nixpkgs { inherit system; };

  deployPkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.deploy-rs.overlays.default
      (_self: super: {
        deploy-rs = {
          inherit (unmodifiedPkgs) deploy-rs;
          lib = super.deploy-rs.lib;
        };
      })
    ];
  };
in
{
  flake = {
    options.deploy.nodes = lib.mkOption {
      type = with lib.types; lazyAttrsOf raw;
    };
    config = {
      lib.deploy-nixos = deployPkgs.deploy-rs.lib.activate.nixos;
      lib.deploy-home-manager = deployPkgs.deploy-rs.lib.activate.home-manager;

      # This is highly advised, and will prevent many possible mistakes
      perSystem =
        { system, ... }:
        {
          checks = (inputs.deploy-rs.lib."${system}").deployChecks self.deploy;
        };
    };
  };
}
