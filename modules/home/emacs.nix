{ flake, config, pkgs, lib, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  cfg = config.my.programs.emacs;

  isolatedOrgBabelConfig = pkgs.writeTextFile {
    name = "isolated-emacs-config.org";
    text = builtins.readFile cfg.orgBabelConfig;
  };

  tangledConfig = pkgs.runCommand "emacs-config-tangled" { } ''
    mkdir $out
    ${lib.getExe pkgs.tangle-emacs-org-babel-config} "${isolatedOrgBabelConfig}" "$out/init.el"
  '';
in
{
  options = {
    my.programs.emacs = {
      enable = lib.mkEnableOption "Install emacs and manage its configuration";
      orgBabelConfig = lib.mkOption {
        default = self + "/users/emacs-config.org";
        type = lib.types.path;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.file.".emacs.d/init.el".source = tangledConfig + "/init.el";
    home.file.".emacs.d/init.elc".source = tangledConfig + "/init.elc";
    home.sessionVariables.EDITOR = "emacsclient -a 'emacs -nw' -nw";
  };
}
