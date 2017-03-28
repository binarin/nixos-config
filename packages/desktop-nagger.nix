{pkgs, ...}:

let
  src = pkgs.fetchFromGitHub {
    owner = "binarin";
    repo = "desktop-nagger";
    rev = "067098ae71acf8fe8d694f8d1fdad56f7668b05a";
    sha256 = "055qr8lla2j3255s13dkjlm5yrpl2hzdd28s9g0l05qpi258v1z8";
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
