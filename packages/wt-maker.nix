{ stdenv, lib, bash, util-linux, git, ... }:

stdenv.mkDerivation rec {
  pname = "wt-maker";
  version = "0.01";
  src = ./wt-maker;
  buildCommand = ''
    mkdir -p $out/bin
    cp $src $out/bin/wt-maker
  '';
}
