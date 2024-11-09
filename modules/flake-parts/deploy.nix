{ inputs, ... }:

let
  self = inputs.self;
  ipam = builtins.fromJSON (builtins.readFile "${self}/ipam.json");
  system = "x86_64-linux";
  unmodifiedPkgs = import inputs.nixpkgs { inherit system; };

  deployPkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.deploy-rs.overlay # or deploy-rs.overlays.default
      (self: super: { deploy-rs = { inherit (unmodifiedPkgs) deploy-rs; lib = super.deploy-rs.lib; }; })
    ];
  };
in
{
  flake = {
    deploy.nodes.forgejo = {
      hostname = ipam.forgejo.interfaces.eth0.address;
      profiles.system = {
        sshUser = "root";
        path = deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations.forgejo;
      };
    };
    # This is highly advised, and will prevent many possible mistakes
    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
  };
}
