{ pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "binarin";
    repo = "desktop-nagger";
    rev = "7b333919d17120a5a49802438ffae03425a45a4e";
    sha256 = "0fi27qdgrmsk31p4wvqnn6ya6p77lgljj1aplmsqbj133xrsh0lx";
  };
  desktop-nagger = pkgs.callPackage src { };
in
{
  options = { };
  config = {
    nixpkgs.config.packageOverrides = super: {
      inherit desktop-nagger;
    };
  };
}
