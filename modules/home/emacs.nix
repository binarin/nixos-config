{ flake, config, pkgs, lib, ... }:
let
  cfg = config.programs.emacs;
in
{
  config = {
    home.file.".emacs.d/init.el".source = cfg.compiledConfig + "/init.el";
    home.file.".emacs.d/init.elc".source = cfg.compiledConfig + "/init.elc";
    fonts.nerdfonts = [ "IosevkaTerm" ];
    home.packages = [
      pkgs.emacs-all-the-icons-fonts
      (pkgs.tree-sitter.withPlugins (p: [ p.tree-sitter-rust ]))
    ];
    home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";
  };
}
