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

  config = lib.mkIf config.hostConfig.feature.impermanence {

    programs.atuin.settings.db_path = "${safeState}/atuin/history.db";

    home.sessionVariables = {
      STARSHIP_CACHE = lib.mkIf config.programs.starship.enable "${garbageDir}/starship";
      GPGHOME = "${safeState}/gnupg";
      IMPERMANENCE_LOCAL_CACHE = "${localCache}";
      _ZO_DATA_DIR = lib.mkIf config.programs.zoxide.enable "${localCache}/zoxide";
    };

    programs.ssh.userKnownHostsFile = lib.mkForce "${safeState}/ssh/known_hosts";
    programs.zsh.dotDir = ".config/zsh";
    programs.zsh.history.path = "${localCache}/zsh";

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
  };
}
