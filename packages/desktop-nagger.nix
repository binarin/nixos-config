{pkgs, ...}:

let
  src = pkgs.fetchFromGitHub {
    owner = "binarin";
    repo = "desktop-nagger";
    rev = "d80a2d438f6960e9973ca9808619213b3d9e6e3b";
    sha256 = "06w29vf4szv44f1zv3plp3cvwgndhklr1kzdhvhvdn4b44bd7zgc";
  };
  desktop-nagger = pkgs.callPackage src {};
in
{
  options = {};
  config = {
    nixpkgs.config.packageOverrides = super: {
      inherit desktop-nagger;
    };
  };
}
