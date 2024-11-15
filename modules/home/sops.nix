{ flake, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  sops = {
    age.keyFile = "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
    defaultSopsFile = "${self}/secrets/${config.inventoryHostName}/user-${config.home.username}.yaml";
  };
}
