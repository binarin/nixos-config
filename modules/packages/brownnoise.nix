let
  packageFn =
    { writeShellApplication, sox }:
    writeShellApplication {
      name = "brownnoise";
      runtimeInputs = [ sox ];
      text = ''
        minutes=''${1:-55}
        exec play -b 16 -r 44k -v 1 -t sl - synth "0:''${minutes}:00" brownnoise band -n 800 500 vol -15db < /dev/zero
      '';
    };
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.brownnoise = pkgs.callPackage packageFn { };
    };

  flake.overlays.brownnoise = final: prev: {
    brownnoise = final.callPackage packageFn { };
  };
}
