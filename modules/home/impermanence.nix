{flake, lib, pkgs, config, osConfig, ...}:
let
  safeDir = "/persist${config.home.homeDirectory}";
  localDir = "/local${config.home.homeDirectory}";
  localCache = "${localDir}/.cache";
  safeState = "${safeDir}/.state";
  garbageDir = "${config.home.homeDirectory}/.garbage";

  symlinkItem = dir: {
    directory = lib.removePrefix config.home.homeDirectory dir;
    method = "symlink";
  };
in {
  imports = [
    flake.inputs.impermanence.homeManagerModules.impermanence
  ];

  options.impermanence = {
    persist-files = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [];
    };
    persist-directories = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [];
    };
    local-files = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [];
    };
    local-directories = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [];
    };
  };

  config = lib.mkIf config.hostConfig.feature.impermanence (
    lib.mkMerge [
      {
        programs.atuin.settings.db_path = "${safeState}/atuin/history.db";

        home.sessionVariables = {
          GPGHOME = "${safeState}/gnupg";
          IMPERMANENCE_LOCAL_CACHE = "${localCache}";
        };

        programs.ssh.userKnownHostsFile = lib.mkForce "${safeDir}/.ssh/known_hosts.d/known_hosts";
        programs.zsh.dotDir = ".config/zsh";
        programs.zsh.history.path = "${localCache}/zsh_history";

        xdg.userDirs = {
          enable = true;
          desktop = "${safeDir}/Desktop";
          documents = "${safeDir}/Documents";
          download = "${safeDir}/Downloads";
          music = "${safeDir}/Music";
          pictures = "${safeDir}/Pictures";
          videos = "${safeDir}/Videos";
        };

        home.persistence."${safeDir}" = {
          enable = true;
          allowOther = true;
        };


        home.persistence."${localDir}" = {
          enable = true;
          allowOther = true;
        };
      }

      (lib.mkIf config.hostConfig.feature.interactive-cli {
        home.persistence."${safeDir}" = {
          files = [
            ".config/sops/age/keys.txt"
          ];
        };
      })

      (lib.mkIf config.programs.starship.enable {
        home.sessionVariables = {
          STARSHIP_CACHE = "${garbageDir}/starship";
        };
      })

      (lib.mkIf config.programs.zoxide.enable {
        home.sessionVariables = {
          _ZO_DATA_DIR = "${localCache}/zoxide";
        };
      })

      (lib.mkIf osConfig.security.pam.services.login.kwallet.enable {
        impermanence.local-directories = [ "${config.xdg.dataHomeRelative}/kwallet" ];
      })
    ]);
}
