{ mkDerivation, stdenv, base, taffybar, safe, hpack, hslogger, cabal-install }:
mkDerivation {
  pname = "taffybar-config";
  version = "0.1.0.0";
  src = ./taffybar-src;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack cabal-install ];
  executableHaskellDepends = [
    base safe taffybar hslogger
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
