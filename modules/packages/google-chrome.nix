{ lib, ... }:
{
  flake.overlays.my-google-chrome = final: prev: {
    google-chrome =
      let
        chromeWrapper = final.writeShellScriptBin "google-chrome-stable" ''
          exec ${lib.getExe prev.google-chrome} --proxy-server=socks5://localhost:3128 "$@"
        '';
      in
      final.buildEnv {
        name = prev.google-chrome.name;
        ignoreCollisions = true;
        paths = [
          chromeWrapper
          prev.google-chrome
        ];
        meta = prev.google-chrome.meta // { mainProgram = "google-chrome-stable"; };
      };
  };
}
