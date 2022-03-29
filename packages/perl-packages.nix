{pkgs, ...}:

with pkgs;
let
in {
  options = {};
  config = {
    environment.systemPackages = with pkgs.perl532Packages; [
      PathTiny
      JSONXS
      JSON
      TextCSV
    ];
  };
}
