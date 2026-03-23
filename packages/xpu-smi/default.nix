{
  fetchurl,
  dpkg,
  autoPatchelfHook,
  stdenv,
  igsc,
  level-zero,
  lib,
  ...
}:
stdenv.mkDerivation {
  pname = "xpu-smi";
  version = "1.3.5";

  src = fetchurl {
    url = "https://github.com/intel/xpumanager/releases/download/v1.3.5/xpu-smi_1.3.5_20251216.170635.605ff78d.u24.04_amd64.deb";
    hash = "sha256-IcgqiQ9XEqZZ08MeqkK8bCxsDQimd9V/vGftoKVMiTQ=";
  };

  nativeBuildInputs = [
    dpkg
    autoPatchelfHook
  ];

  buildInputs = [
    stdenv.cc.cc.lib # libstdc++
    igsc # libigsc.so.0
    level-zero # libze_loader.so.1
  ];

  unpackPhase = ''
    dpkg-deb -x $src .
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib $out/lib/xpu-smi

    cp usr/bin/xpu-smi $out/bin/
    cp usr/lib/x86_64-linux-gnu/libxpum.so.1.3.5 $out/lib/
    ln -s libxpum.so.1.3.5 $out/lib/libxpum.so.1
    ln -s libxpum.so.1 $out/lib/libxpum.so

    cp -r usr/lib/xpu-smi/config $out/lib/xpu-smi/
    cp -r usr/lib/xpu-smi/resources $out/lib/xpu-smi/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Intel XPU System Management Interface";
    homepage = "https://github.com/intel/xpumanager";
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
  };
}
