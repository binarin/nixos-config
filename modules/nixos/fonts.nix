{
  flake,
  lib,
  config,
  ...
}: {
  nixpkgs.overlays = lib.mkIf config.hostConfig.feature.bleeding [
    flake.inputs.self.overlays.fonts-from-unstable
  ];
}
