{ inputs, ... }:
{
  flake-file.inputs.srvos.url = "github:nix-community/srvos";
  flake-file.inputs.srvos.inputs.nixpkgs.follows = "nixpkgs";

  flake.nixosModules.srvos-bits = {
    key = "nixos-config.modules.nixos.srvos-bits";
    imports = [
      "${inputs.srvos}/nixos/common/networking.nix"
      "${inputs.srvos}/nixos/common/nix.nix"
      "${inputs.srvos}/nixos/mixins/mdns.nix"
      "${inputs.srvos}/shared/common/well-known-hosts.nix"
    ];
  };
}
