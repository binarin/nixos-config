{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    system-manager.url = "github:numtide/system-manager";
    system-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
}
