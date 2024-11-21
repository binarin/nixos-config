{ flake, config, pkgs, lib, ... }:
let
  cfg = config.programs.emacs;
in
{
  config = {
    home.file.".emacs.d/init.el".source = cfg.compiledConfig + "/init.el";
    home.file.".emacs.d/init.elc".source = cfg.compiledConfig + "/init.elc";

    home.file.".local/share/applications/org-protocol.desktop".source = pkgs.flakeFile "org-protocol.desktop";
    xdg.mimeAppsdefaultApplications = lib.mkIf pkgs.stdenv.isLinux {
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
    };

    fonts.nerdfonts = [ "IosevkaTerm" ];

    home.packages = [
      pkgs.emacs-all-the-icons-fonts
      (pkgs.tree-sitter.withPlugins (p: [ p.tree-sitter-rust ]))
    ];
    home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";
  };
}
