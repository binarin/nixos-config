{
  flake,
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  config = lib.mkIf config.hostConfig.feature.interactive-cli {
    home.shellAliases = {
      g = "git";
    };

    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      delta.enable = true;
      extraConfig = {
        column.ui = "auto";
        branch.sort = "-committerdate";
        tag.sort = "version:refname";
        diff.algorithm = "histogram";
        diff.mnemonicPrefix = true;

        fetch.prune = true;
        fetch.pruneTags = true;
        fetch.all = true;
        help.autocorrect = "prompt";

        core = {
          autocrlf = false;
        };
        url = {
          "git@github.com:binarin/" = {
            insteadOf = "gh:";
            pushInsteadOf = "gh:";
          };
          "git@forgejo.lynx-lizard.ts.net:binarin/" = {
            insteadOf = "fj:";
            pushInsteadOf = "fj:";
          };
        };
        commit.verbose = true;
        commit.template = "${config.lib.self.file "git-commit-template.txt"}";
        rerere.enabled = true;
        rerere.autoupdate = true;
        merge.conflictstyle = "zdiff3";

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
  };

}
