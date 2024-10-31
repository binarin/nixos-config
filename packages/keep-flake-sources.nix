({ lib, flake, ... }:
let
  inherit (flake) inputs;
in
{
  environment.etc = builtins.listToAttrs (builtins.map
    (input:
      lib.attrsets.nameValuePair "sources/${input}" {
        enable = true;
        source = inputs.${input};
        mode = "symlink";
      })
    (builtins.attrNames inputs));
})
