{
  stdenvNoCC,
  fetchurl,
  unzip,
  lib,
  ...
}:
stdenvNoCC.mkDerivation rec {
  pname = "victoriametrics-datasource";
  version = "v0.10.1";

  src = fetchurl {
    name = "${pname}-${version}.zip";
    hash = "sha256:18aicfrg10w43dgcmk4dpqwvv9l628crmggfh925ahfxcg1nqz8y";
    url = "https://github.com/VictoriaMetrics/victoriametrics-datasource/releases/download/${version}/victoriametrics-datasource-${version}.zip";
  };

  nativeBuildInputs = [unzip];

  installPhase = ''
    cp -R "." "$out"
    chmod -R a-w "$out"
    chmod u+w "$out"
  '';
}
