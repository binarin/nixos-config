{ pkgs, ... }:
with pkgs;
let
in
{
  options = { };
  config = {
    environment.systemPackages = with pkgs.perlPackages; [
      PathTiny
      JSONXS
      JSON
      TextCSV
    ];
  };
}
