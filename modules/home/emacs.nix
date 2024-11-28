{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.programs.emacs;
in
{
  config = {
    xdg.configFile."emacs/init.el".source = cfg.compiledConfig + "/init.el";
    xdg.configFile."emacs/init.elc".source = cfg.compiledConfig + "/init.elc";

    xdg.configFile."emacs/early-init.el".text = ''
      (require 'cl-lib)
      (let ((xdg-cache-home (or (getenv "XDG_CACHE_HOME")
                                (expand-file-name "~/.cache"))))
        (cl-flet ((cache-file-name (&rest components)
                    (apply #'file-name-concat xdg-cache-home "emacs" components)))
          (setq
           package-user-dir (cache-file-name "elpa/"))
           (startup-redirect-eln-cache (cache-file-name "eln-cache/"))))
    '';

    xdg.dataFile."applications/org-protocol.desktop".source = config.lib.self.file "org-protocol.desktop";

    xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
    };

    xdg.dataFile."icons/emacs/org.svg".source = config.lib.self.file "org.svg";

    fonts.nerdfonts = [ "IosevkaTerm" ];

    home.packages = [
      pkgs.emacs-all-the-icons-fonts
      (pkgs.tree-sitter.withPlugins (p: [ p.tree-sitter-rust ]))
    ];
    home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";
  };
}
