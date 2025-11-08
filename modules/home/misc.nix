{ lib, ... }:
{
  config = {
    home.keyboard = lib.mkDefault null;
    home.activation = {
      removeCommonConfictingFiles = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
        $DRY_RUN_CMD rm -fv ~/.gtkrc-2.0 ~/.gtkrc-2.0.backup
      '';
    };
  };
}
