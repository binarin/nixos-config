{
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.wsl {
    home.sessionVariables.LD_LIBRARY_PATH = "/run/opengl-driver/lib/";
  };
}
