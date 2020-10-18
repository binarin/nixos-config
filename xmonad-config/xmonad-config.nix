{ mkDerivation, base, containers, data-default, hpack, lens, stdenv
, stm, taffybar, unix, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-config";
  version = "0.1.0.0";
  src = ./src;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers data-default lens stm unix X11 xmonad
    xmonad-contrib
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
