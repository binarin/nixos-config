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

  local-link-directories-converted = lib.forEach config.impermanence.local-link-directories symlinkItem;
  local-link-directories-no-root-converted = if config.home.username == "root"
                                             then []
                                             else lib.forEach config.impermanence.local-link-directories-no-root symlinkItem;


  persist-link-directories-converted = lib.forEach config.impermanence.persist-link-directories symlinkItem;
  persist-link-directories-no-root-converted = if config.home.username == "root"
                                             then []
                                             else lib.forEach config.impermanence.persist-link-directories-no-root symlinkItem;


in {
  imports = [
    flake.inputs.impermanence.homeManagerModules.impermanence
  ];

  options.impermanence = {
    local-bind-directories = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
    };
    local-bind-directories-no-root = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
    };
    local-link-directories = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [];
    };
    local-link-directories-no-root = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [];
    };
    persist-bind-directories = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [];
    };
    persist-bind-directories-no-root = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [];
    };
    persist-link-directories = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [];
    };
    persist-link-directories-no-root = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
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
          directories = persist-link-directories-converted ++ persist-link-directories-no-root-converted;
          allowOther = true;
        };


        home.persistence."${localDir}" = {
          enable = true;
          directories = local-link-directories-converted ++ local-link-directories-no-root-converted;
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
        impermanence.local-bind-directories-no-root = [ "${config.xdg.dataHome}/kwallet" ];
      })
    ]);
}
