{
  flake,
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;

  isNixOSModule = !(config._module.specialArgs ? osConfig);
  isStandaloneHomemanagerModule = !isNixOSModule && config._module.specialArgs.osConfig != null;
  needOverlays = isNixOSModule || isStandaloneHomemanagerModule;

  cfg = config.programs.emacs;

  cleanup-unicode-from-emacs-org-babel-config = pkgs.writeShellScript "cleanup-unicode-from-emacs-org-babel-config" ''
    ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < "$@"
  '';

  orgBabelConfigWithoutUnicode = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" {} ''
    ${cleanup-unicode-from-emacs-org-babel-config} ${config.lib.self.file cfg.orgBabelConfig} > $out
  '';

  finalEmacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    package = cfg.basePackage;
    config = orgBabelConfigWithoutUnicode;
  };

  compiledConfig = pkgs.runCommand "emacs-config-tangled" {} ''
    mkdir $out
    ${lib.getExe pkgs.tangle-emacs-org-babel-config} "${config.lib.self.file cfg.orgBabelConfig}" "$out/init.el"
  '';
in {
  options = {
    programs.emacs = {
      orgBabelConfig = lib.mkOption {
        default = "emacs-config.org";
        type = lib.types.nonEmptyStr;
      };
      basePackage = lib.mkOption {
        type = lib.types.package;
        default =
          if config.hostConfig.feature.gui
          then pkgs.emacs-pgtk
          else pkgs.emacs-nox;
      };
      compiledConfig = lib.mkOption {
        type = lib.types.pathInStore;
        default = compiledConfig;
      };
      finalEmacsPackage = lib.mkOption {
        type = lib.types.package;
        default = finalEmacsPackage;
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf needOverlays {
      nixpkgs.overlays = [
        flake.inputs.emacs-overlay.overlays.default
        (final: prev: {
          tangle-emacs-org-babel-config = pkgs.writeShellScriptBin "tangle-emacs-org-babel-config" ''
            ${lib.getExe finalEmacsPackage} --batch --load "${config.lib.self.file "byte-compile.el"}" "$@"
          '';
        })
      ];
    })
  ];
}
