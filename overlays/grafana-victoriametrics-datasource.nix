{flake, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
final: prev: {
  grafana-victoriametrics-datasource = final.callPackage "${self}/packages/victoriametrics-datasource.nix" { };
}
