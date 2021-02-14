self: super:

let
  img = super.fetchurl {
    url = "https://github.com/ytmdesktop/ytmdesktop/releases/download/v1.13.0/YouTube-Music-Desktop-App-1.13.0.AppImage";
    sha256 = "0f5l7hra3m3q9zd0ngc9dj4mh1lk0rgicvh9idpd27wr808vy28v";
  };
  run = super.writeShellScriptBin "ytmusic" ''
    exec ${super.appimage-run}/bin/appimage-run ${img}
  '';
in
{
  youtube-music-desktop-app = run;
}
