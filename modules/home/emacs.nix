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
    xdg.configFile."emacs/early-init.el".source = cfg.compiledConfig + "/early-init.el";
    xdg.configFile."emacs/early-init.elc".source = cfg.compiledConfig + "/early-init.elc";

    xdg.dataFile."applications/org-protocol.desktop".source = config.lib.self.file "org-protocol.desktop";

    xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
    };

    xdg.dataFile."icons/emacs/org.svg".source = config.lib.self.file "org.svg";

    fonts.nerdfonts = [ "IosevkaTerm" ];

    home.packages = [
      pkgs.emacs-all-the-icons-fonts
    ];

    home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";
  };
}
