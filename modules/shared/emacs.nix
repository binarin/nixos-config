{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  isNixOSModule = !(config._module.specialArgs ? osConfig);
  isStandaloneHomemanagerModule = !isNixOSModule && config._module.specialArgs.osConfig != null;
  needOverlays = isNixOSModule || isStandaloneHomemanagerModule;

  cfg = config.programs.emacs;

  cleanup-unicode-from-emacs-org-babel-config = pkgs.writeShellScript "cleanup-unicode-from-emacs-org-babel-config" ''
    ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < "$@"
  '';

  orgBabelConfigWithoutUnicode = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
    ${cleanup-unicode-from-emacs-org-babel-config} ${config.lib.self.file cfg.orgBabelConfig} > $out
  '';

  finalEmacsPackage = (pkgs.emacsWithPackagesFromUsePackage {
    extraEmacsPackages = epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
    ];
    package = cfg.basePackage.override (prev: {
      siteStart = pkgs.writeText "site-start.el" (
        (builtins.readFile "${inputs.nixpkgs}/pkgs/applications/editors/emacs/site-start.el")
        + ''
            (let ((dir (getenv "emacsWithPackages_invocationDirectory")))
              (when dir
                (setq invocation-directory (file-name-as-directory dir))
                (setenv "emacsWithPackages_invocationDirectory" nil)))

            (let ((name (getenv "emacsWithPackages_invocationName")))
              (when name
                (setq invocation-name name)
                (setenv "emacsWithPackages_invocationName" nil)))
        ''
      );
    });
    config = orgBabelConfigWithoutUnicode;
  }).overrideAttrs (prev: {
    # Can't get directly to wrapper, it's referenced only as
    # ${./wrapper.sh}. But I can patch substitued wrapper.sh's
    # ${lib.getExe pkgs.perl} -ni -E 'print unless /emacsWithPackages_siteLisp/'
    buildCommand = prev.buildCommand + ''
      for prog in $out/bin/.*-wrapped; do
        sed -i -e "1 aexport emacsWithPackages_invocationDirectory=\"$out/bin\"" "$prog"
        sed -i -e "1 aexport emacsWithPackages_invocationName=\"$(basename "$prog" -wrapped | cut -c2-)\"" "$prog"
      done
    '';
  });

  compiledConfig = pkgs.runCommand "emacs-config-tangled" { } ''
    mkdir $out
    ${lib.getExe pkgs.tangle-emacs-org-babel-config} "${config.lib.self.file cfg.orgBabelConfig}" "$out"
  '';
in
{
  options = {
    programs.emacs = {
      orgBabelConfig = lib.mkOption {
        default = "emacs-config.org";
        type = lib.types.nonEmptyStr;
      };
      basePackage = lib.mkOption {
        type = lib.types.package;
        default = if config.hostConfig.feature.gui then pkgs.emacs-pgtk else pkgs.emacs-nox;
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
          tangle-emacs-org-babel-config = pkgs.writeShellApplication {
            name = "tangle-emacs-org-babel-config";
            runtimeInputs = [
              finalEmacsPackage
            ];
            text = ''
              emacs -q --batch --load "${config.lib.self.file "byte-compile.el"}" "$@"
            '';
          };
        })
      ];
    })
  ];
}
