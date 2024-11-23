{ flake, config, pkgs, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  sops = {
    age.keyFile = "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
    defaultSopsFile = "${config.lib.self.file' "secrets/${config.inventoryHostName}/user-${config.home.username}.yaml"}";
  };
}
