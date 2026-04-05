{
  fetchurl,
  stdenv,
  gnutar,
  gzip,
  ...
}:
stdenv.mkDerivation {
  pname = "sicstus-manual";
  version = "4.10.1";

  nativeBuildInputs = [
    gnutar
    gzip
  ];

  src = fetchurl {
    url = "https://sicstus.sics.se/sicstus/products4/sicstus/4.10.1/binaries/linux/sp-4.10.1-x86_64-linux-glibc2.28.tar.gz";
    sha256 = "1bfqpni23gj5ls3g2mhdv83ma5kgwf6cbwlzyf7qvr5386aicsr7";
  };

  unpackPhase = ''
    tar --extract --gzip --strip-components=2 --file "$src" --wildcards "*/doc/info/sicstus.info*"
  '';

  installPhase = ''
    install -D -t "$out/share/info" info/*
  '';
}
