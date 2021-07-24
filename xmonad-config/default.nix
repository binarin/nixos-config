{ pkgs ? import <nixpkgs> {} }:

rec {
  my-xmonad-config = pkgs.haskellPackages.callPackage ./xmonad-config.nix {};
  my-xmonad-executable = pkgs.stdenv.mkDerivation {
    name = "xmonad-executable";
    phases = [ "installPhase" ];
    installPhase = ''
      ln -sf ${my-xmonad-config}/bin/xmonad-config $out
    '';
  };

  my-taffybar = pkgs.haskellPackages.callPackage ./taffybar-config.nix {};
}
