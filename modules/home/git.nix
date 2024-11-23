{ flake, pkgs, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  home.shellAliases = {
    g = "git";
    # lg = "lazygit";
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Alexey Lebedeff";
    userEmail = "binarin@binarin.info";
    delta.enable = true;
    extraConfig = {
      core = {
        autocrlf = false;
      };
      url = {
        "git@github.com:binarin/" = { insteadOf = "gh:"; pushInsteadOf = "gh:"; };
      };
      commit = {
        template = "${config.lib.self.file "git-commit-template.txt"}";
      };
      "delta \"decorations\"" = {
        commit-decoration-style = "bold yellow box ul";
        file-style = "bold yellow ul";
        file-decoration-style = "none";
      };
      init = {
        defaultBranch = "master";
      };
    };
  };
}
