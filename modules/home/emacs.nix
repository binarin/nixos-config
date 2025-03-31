{ flake, config, pkgs, lib, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  cfg = config.programs.custom-emacs;

  cleanup-unicode-from-emacs-org-babel-config = pkgs.writeShellScript "cleanup-unicode-from-emacs-org-babel-config" ''
    ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < "$@"
  '';

  orgBabelConfigWithoutUnicode = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
    ${cleanup-unicode-from-emacs-org-babel-config} ${config.lib.self.file cfg.orgBabelConfig} > $out
  '';

  kdl-ts-mode = {melpaBuild}: melpaBuild {
    pname = "kdl-ts-mode";
    version = "20240106.01";
    src = pkgs.fetchFromGitHub {
      owner = "dataphract";
        repo = "kdl-ts-mode";
        rev = "3dbf116cd19261d8d70f456ae3385e1d20208452";
        hash = "sha256-4bfKUzzLhBFg4TeGQD0dClumcO4caIBU8/uRncFVVFQ=";
    };
  };

  finalEmacsPackage = (pkgs.emacsWithPackagesFromUsePackage {
    override = epkgs: epkgs // {
      kdl-ts-mode = kdl-ts-mode {inherit (epkgs) melpaBuild; };
    };
    extraEmacsPackages = epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
      lsp-bridge
      # emacs-lsp-booster
    ];
    package = cfg.basePackage.override (prev: {
      # XXX Okay, this is the reason for regular emacs rebuild - but the fix wasn't merged in 24.11
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

  tangle-emacs-org-babel-config = pkgs.writeShellApplication {
    name = "tangle-emacs-org-babel-config";
    runtimeInputs = [
      finalEmacsPackage
    ];
    text = ''
      emacs -q --batch --load "${config.lib.self.file "byte-compile.el"}" "$@"
    '';
  };

  compiledConfig = pkgs.runCommand "emacs-config-tangled" { } ''
    mkdir $out
    ${lib.getExe tangle-emacs-org-babel-config} "${config.lib.self.file cfg.orgBabelConfig}" "$out"
  '';
in
{
  options = {
    programs.custom-emacs = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = config.hostConfig.feature.emacs;
      };

      orgBabelConfig = lib.mkOption {
        default = "emacs-config.org";
        type = lib.types.nonEmptyStr;
      };

      basePackage = lib.mkOption {
        type = lib.types.package;
        default = if config.hostConfig.feature.gui then pkgs.emacs-pgtk-nixpkgs else pkgs.emacs-nox;
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

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      impermanence.persist-bind-directories-no-root = [ "org" ];

      xdg.configFile."emacs/init.el".source = cfg.compiledConfig + "/init.el";
      xdg.configFile."emacs/init.elc".source = cfg.compiledConfig + "/init.elc";
      xdg.configFile."emacs/early-init.el".source = cfg.compiledConfig + "/early-init.el";
      xdg.configFile."emacs/early-init.elc".source = cfg.compiledConfig + "/early-init.elc";

      home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";

      home.packages = [
        finalEmacsPackage
        tangle-emacs-org-babel-config
      ];
    }

    (lib.mkIf config.hostConfig.feature.gui {
      xdg.dataFile."applications/org-protocol.desktop".source = config.lib.self.file "org-protocol.desktop";

      xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      };

      xdg.dataFile."icons/emacs/org.svg".source = config.lib.self.file "org.svg";

      fonts.nerdfonts = [ "IosevkaTerm" ];

      home.packages = [
        pkgs.emacs-all-the-icons-fonts
      ];
    })
  ]);
}
