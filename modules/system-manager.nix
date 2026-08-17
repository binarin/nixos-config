{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    system-manager.url = "github:numtide/system-manager/release-26.05";
    system-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
}
