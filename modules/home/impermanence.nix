{flake, lib, pkgs, config, ...}:
let
  safeDir = "/persist${config.home.homeDirectory}";
  localCache = "/local${config.home.homeDirectory}/.cache";
  safeState = "${safeDir}/.state";
  garbageDir = "${config.home.homeDirectory}/.garbage";
in {
  imports = [
    flake.inputs.impermanence.homeManagerModules.impermanence
  ];

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

        home.persistence."${localCache}" = {
          enable = true;
          directories = [
            ".mozilla"
            ".thunderbird"
            (lib.removePrefix config.home.homeDirectory "${config.xdg.dataHome}/direnv")
          ];
          allowOther = true;
        };
      }
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
    ]);
}
