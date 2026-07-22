{ inputs, ... }:
{
  flake.overlays.wprintidle-c =
    final: prev:
    {
      wprintidle-c = final.stdenv.mkDerivation {
        pname = "wprintidle-c";
        version = "0.1.0";
        src = inputs.wprintidle-c;

        nativeBuildInputs = [
          final.wayland-scanner
          final.pkg-config
        ];
        buildInputs = [ final.wayland ];

        # The Makefile's `install` target hardcodes /usr paths, so don't use it.
        dontUseMakeInstall = true;

        buildFlags = [ "CC=${final.stdenv.cc.targetPrefix}cc" ];

        installPhase = ''
          runHook preInstall
          install -Dm755 wprintidle-c-daemon $out/bin/wprintidle-c-daemon
          install -Dm755 wprintidle-c      $out/bin/wprintidle-c
          runHook postInstall
        '';

        meta.mainProgram = "wprintidle-c";
      };
    };
}
