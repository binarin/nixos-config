{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    emacs-overlay.url = "emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";
  };

  flake.nixosModules.emacs =
    {
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.emacs";

      nixpkgs.overlays = [
        inputs.emacs-overlay.overlays.default
      ];

      environment.systemPackages = with pkgs; [
        emacs-nox # XXX add some zero-conf modes?
      ];
    };

  flake.homeModules.emacs =
    {
      config,
      pkgs,
      lib,
      osConfig,
      ...
    }:
    let
      cfg = config.programs.custom-emacs;

      cleanup-unicode-from-emacs-org-babel-config = pkgs.writeShellScript "cleanup-unicode-from-emacs-org-babel-config" ''
        ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < "$@"
      '';

      orgBabelConfigWithoutUnicode = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
        ${cleanup-unicode-from-emacs-org-babel-config} ${config.lib.self.file cfg.orgBabelConfig} > $out
      '';

      kbd-mode-builder =
        { trivialBuild }:
        trivialBuild {
          pname = "kbd-mode";
          version = "20250222.01";
          src = pkgs.fetchurl {
            hash = "sha256-h2on6BIXUNqWsp1DeI8cIJyyBG5ijuF77ql3b6WXAq8=";
            url = "https://github.com/kmonad/kbd-mode/raw/a349015860fccd31b0c56147d7fa641b68afa07f/kbd-mode.el";
          };
        };

      finalEmacsPackage =
        (pkgs.emacsWithPackagesFromUsePackage {
          override =
            epkgs:
            epkgs
            // {
              kbd-mode = kbd-mode-builder { inherit (epkgs) trivialBuild; };
            };
          extraEmacsPackages =
            epkgs: with epkgs; [
              treesit-grammars.with-all-grammars
              kbd-mode
            ];
          package = cfg.basePackage;
          config = orgBabelConfigWithoutUnicode;
        }).overrideAttrs
          (prev: {
            # Can't get directly to wrapper, it's referenced only as
            # ${./wrapper.sh}. But I can patch substitued wrapper.sh's
            # ${lib.getExe pkgs.perl} -ni -E 'print unless /emacsWithPackages_siteLisp/'
            buildCommand = prev.buildCommand + ''
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
                done
              fi
            '';
          });

      tangle-emacs-org-babel-config = pkgs.writeShellApplication {
        name = "tangle-emacs-org-babel-config";
        runtimeInputs = [ finalEmacsPackage ];
        text = ''
          emacs -q --batch --load "${config.lib.self.file "byte-compile.el"}" "$@"
        '';
      };

      # XXX find a better way to handle org-includes, while still preserving
      compiledConfig = pkgs.runCommand "emacs-config-tangled" { } ''
        echo $srcs
        mkdir $out
        cd $out
        cp "${config.lib.self.file "pta.el"}" pta.el
        cp "${config.lib.self.file cfg.orgBabelConfig}" emacs-config.org
        ${lib.getExe tangle-emacs-org-babel-config} emacs-config.org "$out"
        rm pta.el emacs-config.org
      '';
    in
    {
      key = "nixos-config.modules.home.emacs";

      options = {
        # XXX not needed, rename to to homeModules.emacs-binarin
        programs.custom-emacs = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
          };

          orgBabelConfig = lib.mkOption {
            default = "emacs-config.org";
            type = lib.types.nonEmptyStr;
          };

          basePackage = lib.mkOption {
            type = lib.types.package;
            default = if osConfig.services.graphical-desktop.enable then pkgs.emacs-pgtk else pkgs.emacs-nox;
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

      config = lib.mkIf cfg.enable (
        lib.mkMerge [
          {
            programs.emacs = {
              enable = lib.mkForce false; # finalEmacsPackage is configured and installed in this file
            };

            services.emacs.package = finalEmacsPackage;

            home.packages = with pkgs; [
              finalEmacsPackage
              tangle-emacs-org-babel-config
              wtype
            ];
            xdg.configFile."emacs/init.el".source = cfg.compiledConfig + "/init.el";
            xdg.configFile."emacs/init.elc".source = cfg.compiledConfig + "/init.elc";
            xdg.configFile."emacs/early-init.el".source = cfg.compiledConfig + "/early-init.el";
            xdg.configFile."emacs/early-init.elc".source = cfg.compiledConfig + "/early-init.elc";
          }

          (lib.mkIf osConfig.services.graphical-desktop.enable {
            xdg.dataFile."applications/org-protocol.desktop".source =
              config.lib.self.file "org-protocol.desktop";

            xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
              "x-scheme-handler/org-protocol" = "org-protocol.desktop";
            };

            xdg.dataFile."icons/emacs/org.svg".source = config.lib.self.file "org.svg";

            home.packages = with pkgs; [
              emacs-all-the-icons-fonts
              ghostscript
            ];
          })
        ]
      );
    };
}
