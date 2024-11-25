{
  config,
  lib,
  flake,
  pkgs,
  ...
}:
let
  inherit (flake) inputs;

  walkInputs =
    fl:
    let
      selfAttrs = lib.optionalAttrs (lib.isStringLike fl) { self = "${fl}"; };
    in
    if builtins.isAttrs fl && fl ? "inputs" then
      (lib.mapAttrs (name: value: walkInputs value) fl.inputs) // selfAttrs
    else
      selfAttrs;

  flakeInputsRecursive =
    fl:
    lib.mapAttrsRecursive (dir: path: {
      _type = "leaf";
      filename = "sources/${lib.concatStringsSep "/" dir}";
      source = path;
    }) (walkInputs fl);

  allInputs = fl: lib.collect (x: x ? _type && x._type == "leaf") (flakeInputsRecursive fl);

  flake-all-sources-keeper =
    pkgs:
    pkgs.writeShellScriptBin "flake-all-sources-keeper" ''
      cat <<'EOF'
      ${lib.concatStringsSep "\n" (
        lib.map ({ filename, source, ... }: "${filename}: ${source}") (allInputs flake)
      )}
      ${pkgs.hjson} # hello, home-manager broot module
      EOF
    '';
in
{
  config = lib.mkMerge [
    {
      nixpkgs.overlays = [
        (final: prev: { flake-all-sources-keeper = flake-all-sources-keeper final; })
      ];
    }
    (lib.mkIf config.hostConfig.feature.nix-builder {
      environment.etc."nix/flake-all-sources-keeper".source = "${pkgs.flake-all-sources-keeper}";
      nix.extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
      '';
    })
  ];
}
