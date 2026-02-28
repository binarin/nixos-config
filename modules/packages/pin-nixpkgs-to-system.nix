{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.pin-nixpkgs-to-system = pkgs.python3Packages.buildPythonApplication {
        pname = "pin-nixpkgs-to-system";
        version = "0.1.0";
        format = "other";

        src = ../../files/pin-nixpkgs-to-system.py;

        dontUnpack = true;
        dontBuild = true;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        installPhase = ''
          install -Dm755 $src $out/bin/pin-nixpkgs-to-system
          wrapProgram $out/bin/pin-nixpkgs-to-system \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nix ]}
        '';
      };
    };
}
