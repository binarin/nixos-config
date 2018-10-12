{
  network.description = "Deployment for CD pipeline for amon";
  amon = {config, lib, pkgs, ...}: {
    imports = [
      ../configuration.nix-amon
    ];
  };
}
