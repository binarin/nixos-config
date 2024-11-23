{ flake, pkgs, lib, config, null, specialArgs, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  isNixOSorDarwin = !(specialArgs ? osConfig);
  isStandaloneHomeManager = specialArgs ? osConfig && specialArgs.osConfig != null;
in
{
  config = {
    lib.self = {
      file = name: pkgs.writeText name (builtins.readFile "${self}/files/${name}");
      read = name: builtins.readFile "${self}/files/${name}";
    };

    nixpkgs.overlays = lib.mkIf (isNixOSorDarwin || isStandaloneHomeManager) [
      (final: prev:
        let
          mkMaker = builder: fileName:
            let
              bn = builtins.baseNameOf fileName;
            in
            if fileName != bn
            then throw "There should be no directory parts in flakeFile name - '${fileName}'"
            else builder "${bn}" (builtins.readFile "${self}/files/${bn}");
        in
        {
          flakeReadFile = mkMaker (name: content: content);
          flakeFile = mkMaker final.writeText;
          flakeScript = mkMaker final.writeScript;
          flakeScriptBin = mkMaker final.writeScriptBin;
        })
    ];
  };
}
