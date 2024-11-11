{ flake, pkgs, lib, config, null, specialArgs, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  isNixOSorDarwin = !(specialArgs ? osConfig);
  isStandaloneHomeManager = specialArgs ? osConfig && specialArgs.osConfig != null;
in
{
  config = {
    nixpkgs.overlays = lib.mkIf (isNixOSorDarwin || isStandaloneHomeManager) [
      (final: prev:
        let
          mkMaker = { builder, builderName }: fileName:
            let
              bn = builtins.baseNameOf fileName;
            in
            if fileName != bn
            then throw "There should be no directory parts in flakeFile name - '${fileName}'"
            else
              builder {
                name = "${builderName}-${bn}";
                text = builtins.readFile "${self}/files/${bn}";
              };
        in
        {
          flakeFile = mkMaker { builder = final.writeTextFile; builderName = "flake-file"; };
          flakeScript = mkMaker { builder = final.writeShellScript; builderName = "flake-script"; };
          flakeScriptBin = mkMaker { builder = final.writeShellScriptBin; builderName = "flake-script-bin"; };
        })
    ];
  };
}
