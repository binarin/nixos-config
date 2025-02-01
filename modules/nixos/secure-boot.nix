{flake, lib, pkgs, config, ...}:
{
  config = lib.mkIf config.hostConfig.feature.secure-boot {
  };
}
