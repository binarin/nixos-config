{pkgs, ...}:
with pkgs; let
in {
  options = {};
  config = {
    environment.systemPackages = with pkgs.perl536Packages; [
      PathTiny
      JSONXS
      JSON
      TextCSV
    ];
  };
}
