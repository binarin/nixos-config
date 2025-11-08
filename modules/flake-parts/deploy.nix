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

  # NOTE: can scrape nixosConfigurations, but computation is a bit too heavy
  deployableSystems = [
    "forgejo"
    "monitor"
    "media"
    "nix-cache"
    "mail"
    "docker-on-nixos"
  ];

  deployNixosSystem =
    hostName:
    let
      deployHostName = self.nixosConfigurations."${hostName}".config.hostConfig.deployHostName;
    in
    {
      hostname = deployHostName;
      profiles.system = {
        sshUser = "root";
        path = deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations."${hostName}";
      };
    };
in
{
  flake = {
    options.deploy.nodes = lib.mkOption {
      type = with lib.types; lazyAttrsOf raw;
    };
    config = {
      lib.deploy-nixos = deployPkgs.deploy-rs.lib.activate.nixos;

      deploy.nodes = lib.genAttrs deployableSystems deployNixosSystem;

      # This is highly advised, and will prevent many possible mistakes
      perSystem =
        { system, ... }:
        {
          checks = (inputs.deploy-rs.lib."${system}").deployChecks self.deploy;
        };
    };
  };
}
