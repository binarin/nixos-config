{flake, config, ...}:
{
  sops = {
    age.keyFile = "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
    defaultSopsFile = flake.inputs.self.lib.hostConfig.userSopsFile config.inventoryHostName config.home.username;
  };
}
