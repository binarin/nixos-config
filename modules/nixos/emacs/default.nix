{ flake, config, pkgs, lib, ... }:

let
  inherit (flake.inputs) self;
  cfg = config.my.programs.emacs;
  orgBabelConfigWithoutUnicode = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
    ${lib.getExe pkgs.cleanup-unicode-from-emacs-org-babel-config} ${cfg.orgBabelConfig} > $out
  '';
  finalEmacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    package = cfg.basePackage;
    config = orgBabelConfigWithoutUnicode;
  };
in
{
  options = {
    my.programs.emacs = {
      enable = lib.mkOption {
        description = "Whether to install emacs system-wide";
        default = true;
        type = lib.types.bool;
      };
      basePackage = lib.mkOption {
        type = lib.types.package;
        default = if config.hostOptions.gui.enable then pkgs.emacs-pgtk else pkgs.emacs-nox;
      };
      orgBabelConfig = lib.mkOption {
        type = lib.types.path;
        default = self + "/users/emacs-config.org";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      finalEmacsPackage
      pkgs.tangle-emacs-org-babel-config
    ];
    nixpkgs.overlays = [
      (final: prev: {
        cleanup-unicode-from-emacs-org-babel-config = pkgs.callPackage (self + "/packages/cleanup-unicode-from-emacs-org-babel-config.nix") { };
        tangle-emacs-org-babel-config = pkgs.callPackage (self + "/packages/tangle-emacs-org-babel-config.nix") {
          emacs = finalEmacsPackage;
        };
      })
    ];
  };
}
