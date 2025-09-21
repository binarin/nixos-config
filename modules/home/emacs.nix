{ flake, config, pkgs, lib, hostConfig, ... }:
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

  kbd-mode-builder = {trivialBuild}: trivialBuild {
    pname = "kbd-mode";
    version = "20250222.01";
    src = pkgs.fetchurl {
      hash = "sha256-h2on6BIXUNqWsp1DeI8cIJyyBG5ijuF77ql3b6WXAq8=";
      url = "https://github.com/kmonad/kbd-mode/raw/a349015860fccd31b0c56147d7fa641b68afa07f/kbd-mode.el";
    };
  };

  finalEmacsPackage = (pkgs.emacsWithPackagesFromUsePackage {
    override = epkgs: epkgs // {
      kbd-mode = kbd-mode-builder {inherit (epkgs) trivialBuild; };
    };
    extraEmacsPackages = epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
      kbd-mode
    ];
    package = cfg.basePackage;
    config = orgBabelConfigWithoutUnicode;
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
        default = if config.hostConfig.feature.gui then pkgs.emacs else pkgs.emacs-nox;
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
      home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";

      home.packages = [
        finalEmacsPackage
        tangle-emacs-org-babel-config
      ];
    }
    (lib.optionalAttrs hostConfig.isLinux {
      impermanence.persist-bind-directories-no-root = [ "org" ];

      xdg.configFile."emacs/init.el".source = cfg.compiledConfig + "/init.el";
      xdg.configFile."emacs/init.elc".source = cfg.compiledConfig + "/init.elc";
      xdg.configFile."emacs/early-init.el".source = cfg.compiledConfig + "/early-init.el";
      xdg.configFile."emacs/early-init.elc".source = cfg.compiledConfig + "/early-init.elc";
    })

    (lib.optionalAttrs hostConfig.isDarwin {
      home.file.".emacs.d/init.el".source = cfg.compiledConfig + "/init.el";
      home.file.".emacs.d/init.elc".source = cfg.compiledConfig + "/init.elc";
      home.file.".emacs.d/early-init.el".source = cfg.compiledConfig + "/early-init.el";
      home.file."emacs/early-init.elc".source = cfg.compiledConfig + "/early-init.elc";

      home.packages = with pkgs; [ terminal-notifier ];
    })

    (lib.mkIf config.hostConfig.feature.gui {
      xdg.dataFile."applications/org-protocol.desktop".source = config.lib.self.file "org-protocol.desktop";

      xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      };

      xdg.dataFile."icons/emacs/org.svg".source = config.lib.self.file "org.svg";

      home.packages = with pkgs; [
        emacs-all-the-icons-fonts
        ghostscript
      ];
    })
  ]);
}
