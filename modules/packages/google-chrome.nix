{ lib, ... }:
{
  flake.overlays.my-google-chrome = final: prev: {
    google-chrome =
      let
        chromeWrapper = final.writeShellScriptBin "google-chrome-stable" ''
          if [[ -n "''${CHROME_PROXY:-}" ]]; then
            exec ${lib.getExe prev.google-chrome} --proxy-server="$CHROME_PROXY" "$@"
          else
            exec ${lib.getExe prev.google-chrome} "$@"
          fi
        '';

        chromeBase = final.symlinkJoin {
          name = prev.google-chrome.name;
          paths = [
            chromeWrapper
            prev.google-chrome
          ];
          meta = prev.google-chrome.meta // { mainProgram = "google-chrome-stable"; };
        };
      in
      final.wrapShittyShit chromeBase {
        # Screen sharing goes through the XDG ScreenCast portal: Chrome asks
        # org.freedesktop.portal.Desktop for a PipeWire node; the actual video
        # then flows over PipeWire's own socket, not D-Bus.
        talk = [ "org.freedesktop.portal.Desktop" ];
        own = [ ];
        see = [ ];
      };
  };
}
