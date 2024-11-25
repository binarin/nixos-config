{
  flake,
  lib,
  ...
}: {
  imports =
    [
      flake.inputs.self.sharedModules.default
    ]
    ++ lib.attrValues (lib.removeAttrs flake.inputs.self.homeModules ["default"]);

  config = {
    home.stateVersion = lib.mkDefault "24.05";
    home.keyboard = lib.mkDefault null;
    home.activation = {
      removeCommonConfictingFiles = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
        $DRY_RUN_CMD rm -fv ~/.gtkrc-2.0 ~/.gtkrc-2.0.backup
      '';
    };
  };
}
