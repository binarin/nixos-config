{ flake, pkgs, lib, config, null, specialArgs, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  isNixOSorDarwin = !(specialArgs ? osConfig);
  isStandaloneHomeManager = specialArgs ? osConfig && specialArgs.osConfig != null;
in
{
  lib.self = let
    read = name: builtins.readFile "${self}/files/${name}";
  in {
    inherit read;
    file = name: pkgs.writeText name (read name);
    script = name: pkgs.writeScript name (read name);
    scriptBin = name: pkgs.writeScriptBin name (read name);
  };
}
