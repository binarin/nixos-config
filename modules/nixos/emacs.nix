{
  flake,
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;
in {
  imports = [self.sharedModules.emacs];

  environment.systemPackages = [
    config.programs.emacs.finalEmacsPackage
    pkgs.tangle-emacs-org-babel-config
  ];

  # home-manager.users = lib.genAttrs config.hostConfig.managedUsers (user: {
  # });
}
