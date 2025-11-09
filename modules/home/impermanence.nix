{
  flake,
  lib,
  config,
  osConfig,
  ...
}:
let
  homeDir = config.home.homeDirectory;
  safeDir = "/persist${homeDir}";
  localDir = "/local${homeDir}";
  localCache = "${localDir}/.cache";
  safeState = "${safeDir}/.state";
  garbageDir = "${config.home.homeDirectory}/.garbage";
in
{
  imports = [
    flake.inputs.impermanence.homeManagerModules.impermanence
  ];

  options.impermanence = {
    persist-files = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [ ];
    };
    persist-directories = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [ ];
    };
    local-files = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [ ];
    };
    local-directories = lib.mkOption {
      type = with lib.types; listOf (either str (lazyAttrsOf raw));
      default = [ ];
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

        programs.zsh.dotDir = ".config/zsh";
        programs.zsh.history.path = "${localCache}/zsh_history";

        xdg = {
          enable = true;
          stateHome = "${safeDir}/.local/state";
        };

        impermanence.persist-directories = [
          "Desktop"
          "Documents"
          "Downloads"
          "Music"
          "Pictures"
          "Videos"
        ];

        xdg.userDirs = {
          enable = true;
          desktop = "${homeDir}/Desktop";
          documents = "${homeDir}/Documents";
          download = "${homeDir}/Downloads";
          music = "${homeDir}/Music";
          pictures = "${homeDir}/Pictures";
          videos = "${homeDir}/Videos";
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
        impermanence.local-directories = [ ".local/share/kwallet" ];
      })
    ]
  );
}
