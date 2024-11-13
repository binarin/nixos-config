{ lib, flake, ... }:
let
  inherit (flake) inputs;
in
{
  config = lib.mkIf config.hostConfig.feature.nix-builder {
    environment.etc = builtins.listToAttrs (builtins.map
      (input:
        lib.attrsets.nameValuePair "sources/${input}" {
          enable = true;
          source = inputs.${input};
          mode = "symlink";
        })
      (builtins.attrNames inputs));

    nix.extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';
  };
}
