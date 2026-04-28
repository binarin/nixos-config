{ lib, ... }:
{
  flake.overlays.my-google-chrome = final: prev: {
    google-chrome =
      let
        chromeWrapper = final.writeShellScriptBin "google-chrome-stable" ''
          exec ${lib.getExe prev.google-chrome} --proxy-server=socks5://localhost:3128 "$@"
        '';

        desktopFiles = final.runCommand "google-chrome-desktop-files" { } ''
          mkdir -p "$out/share/applications"
          for f in ${prev.google-chrome}/share/applications/*.desktop; do
            sed "s|/nix/store/[^-]*-google-chrome-[^/]*/bin/google-chrome-stable|${chromeWrapper}/bin/google-chrome-stable|g" "$f" > "$out/share/applications/$(basename "$f")"
          done
        '';
      in
      final.symlinkJoin {
        name = prev.google-chrome.name;
        paths = [
          chromeWrapper
          desktopFiles
          prev.google-chrome
        ];
        meta = prev.google-chrome.meta // { mainProgram = "google-chrome-stable"; };
      };
  };
}
