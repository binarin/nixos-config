let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          # xmonad = haskellPackagesNew.callPackage ./xmonad.nix;
          # xmonad-contrib = haskellPackagesNew.callPackage ./xmonad-contrib.nix;
          xmonad-config = haskellPackagesNew.callPackage ./xmonad-config.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in {
  xmonad-config = pkgs.haskellPackages.xmonad-config;
  xmonad-executable = pkgs.stdenv.mkDerivation {
    name = "xmonad-executable";
    phases = [ "installPhase" ];
    installPhase = ''
      ln -sf ${pkgs.haskellPackages.xmonad-config}/bin/xmonad-config $out
    '';
  };
}
