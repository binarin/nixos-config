{
  self,
  ...
}:
{
  flake.homeModules.git =
    {
      pkgs,
      config,
      lib,
      osConfig,
      ...
    }:
    {
      key = "nixos-config.modules.home.git";

      config = {
        home.shellAliases = {
          g = "git";
        };

        programs.git = {
          enable = true;
          package = if osConfig.services.graphical-desktop.enable then pkgs.gitAndTools.gitFull else pkgs.git;
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
    };

  flake.nixosModules.git =
    { ... }:
    {
      key = "nixos-config.modules.nixos.git";
      programs.git.enable = true;
    };

}
