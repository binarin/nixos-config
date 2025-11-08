{
  flake,
  config,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [ inputs.sops-nix.homeManagerModules.sops ];

  sops = {
    age.keyFile = "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
    defaultSopsFile = config.lib.self.optionalFile' "secrets/${config.inventoryHostName}/user-${config.home.username}.yaml";
  };
}
