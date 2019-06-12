{pkgs, ...}:

with pkgs;
let
in {
  options = {};
  config = {
    environment.systemPackages = with pkgs.perl528Packages; [
      PathTiny
      JSONXS
      JSON
    ];
  };
}
