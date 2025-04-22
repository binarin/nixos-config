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
      withNativeCompilation = pkgs.stdenv.isLinux;
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

      cat << 'EOF' > env-sourcer
      if [[ -z "''${__ETC_BASHRC_SOURCED-}" && -f /etc/bashrc ]]; then
        . /etc/bashrc
      fi

      if [[ -z "''${__HM_SESS_VARS_SOURCED-}" && -f $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh ]]; then
        . $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
      fi
      EOF

      if [ -d "$out/Applications/Emacs.app" ]; then
        for prog in $out/Applications/Emacs.app/Contents/MacOS/.*-wrapped; do
          sed -i -e "2 e cat env-sourcer" "$prog"
          sed -i -e "1 aexport emacsWithPackages_invocationDirectory=\"$out/bin\"" "$prog"
          sed -i -e "1 aexport emacsWithPackages_invocationName=\"$(basename "$prog" -wrapped | cut -c2-)\"" "$prog"
        done
      fi




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
        default = if config.hostConfig.feature.gui then pkgs.emacs-saved else pkgs.emacs-git-nox;
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
    })

    (lib.mkIf config.hostConfig.feature.gui {
      xdg.dataFile."applications/org-protocol.desktop".source = config.lib.self.file "org-protocol.desktop";

      xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      };

      xdg.dataFile."icons/emacs/org.svg".source = config.lib.self.file "org.svg";

      home.packages = [
        pkgs.emacs-all-the-icons-fonts
      ];
    })
  ]);
}
