{ flake, config, pkgs, lib, hostConfig, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  cfg = config.programs.custom-emacs;

  cleanup-unicode-from-emacs-org-babel-config = pkgs.writeShellScript "cleanup-unicode-from-emacs-org-babel-config" ''
    ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < "$@"
  '';

  hledger-emacs-compat = pkgs.writeShellScriptBin "hledger-emacs-compat" ''
    # hledger.sh
    # Kudos to acarrico for the original script.

    # The script is contingent on ledger-default-date-string
    # being set to "%Y-%m-%d", aka ISO date format.

    iargs=("$@")
    oargs=()
    j=0;
    date=;
    for((i=0; i<''${#iargs[@]}; ++i)); do
        case ''${iargs[i]} in
            --date-format)
                # drop --date-format and the next arg
                i=$((i+1));
                ;;
            cleared) # for ledger-di
                splay-balance-at-point
                # convert "cleared" to "balance -N -C"
                oargs[j]=balance; oargs[j+1]=-N; oargs[j+2]=-C; j=$((j+3));
                ;;
            xact)
                # convert "xact" to "print --match"
                oargs[j]=print; oargs[j+1]=--match; j=$((j+2));
                # drop xact argument and stash the date argument
                i=$((i+1));
                date=''${iargs[i]};
                ;;
            # NOTE: Reconciliation still doesn't work for other reasons
            #       so the following filters/conversions are unnecessary for now.
            # --sort) # for reconcilliation
            #     # drop --sort and the next arg
            #     i=$((i+1));
            #     ;;
            # --uncleared) # for reconcilliation
            #     # convert "--uncleared" to "--unmarked --pending"
            #     oargs[j]=--unmarked; oargs[j+1]=--pending; j=$((j+2))
            #     ;;
            *)
                # keep any other args:
                oargs[j]=''${iargs[i]};
                j=$((j+1));
                ;;
        esac
    done

    if test "$date"
    then
        # substitute the given date for the old date:
        hledger "''${oargs[@]}" | sed "1s/....-..-../$date/"
    else
        # echo "''${oargs[@]}"
        hledger "''${oargs[@]}"
    fi
  '';

  orgBabelConfigWithoutUnicode = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
    ${cleanup-unicode-from-emacs-org-babel-config} ${config.lib.self.file cfg.orgBabelConfig} > $out
  '';

  finalEmacsPackage = (pkgs.emacsWithPackagesFromUsePackage {
    override = epkgs: epkgs // {
      # kdl-ts-mode = kdl-ts-mode {inherit (epkgs) melpaBuild; };
    };
    extraEmacsPackages = epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
      # lsp-bridge
      # emacs-lsp-booster
    ];
    package = cfg.basePackage;
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
