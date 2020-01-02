{pkgs, stdenv, fetchurl, iptables, xz, utillinux, e2fsprogs, iproute, makeWrapper, ...}:

stdenv.mkDerivation {
  name = "docker-1.11.2";
  src = pkgs.fetchurl {
    url = "https://get.docker.com/builds/Linux/x86_64/docker-1.11.2.tgz";
    sha256 = "8c2e0c35e3cda11706f54b2d46c2521a6e9026a7b13c7d4b8ae1f3a706fc55e1";
  };
  buildInputs = [ makeWrapper iptables xz utillinux e2fsprogs iproute ];
  installPhase = ''
    mkdir -p $out/bin $out/libexec/docker
    cp * $out/libexec/docker
    makeWrapper $out/libexec/docker/docker $out/bin/docker \
      --prefix PATH : "$out/libexec/docker:${iproute}/sbin:sbin:${iptables}/sbin:${e2fsprogs}/sbin:${xz}/bin:${utillinux}/bin"
   '';
}
