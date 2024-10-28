{ config, lib, ... }:
{
  options = {
    gui.enable = lib.mkEnableOption "Whether to enable gui-related stuff";
  };
}
