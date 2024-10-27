self: super:
let
  nixpkgs = import ../nixpkgs-master {
    overlays = [ ]; # prevent further layering
    config = {
      allowUnfree = true;
    };
  };
in
{
  bleeding = nixpkgs.pkgs;
}
