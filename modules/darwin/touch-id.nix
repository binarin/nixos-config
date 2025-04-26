{flake, pkgs, lib, config, ...}:
let
in {
  disabledModules = [
    "${flake.inputs.nix-darwin}/modules/security/pam.nix"
  ];
  imports = [
    "${flake.inputs.nix-darwin-master}/modules/security/pam.nix"
  ];
  config = {
    security.pam.services.sudo_local = {
      enable = true;
      touchIdAuth = true;
      reattach = true;
    };
  };
}
