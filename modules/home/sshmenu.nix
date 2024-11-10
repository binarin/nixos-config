{flake, pkgs, lib, config, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  home.packages = [ pkgs.rxvt-unicode ];
  home.file."bin/sshmenu".source = pkgs.writeTextFile {
    name = "sshmenu";
    text = builtins.readFile ./sshmenu;
    executable = true;
  };
}
