{ ... }:
{
  flake-file.inputs.nixos-raspberrypi = {
    url = "github:nvmd/nixos-raspberrypi/develop";
    inputs.nixpkgs.follows = "nixpkgs";
  };
}
