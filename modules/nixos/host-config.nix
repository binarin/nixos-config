{
  flake,
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in
{
  networking.hostName = config.inventoryHostName;
  networking.hostId = config.hostConfig.hostId;
  nix.settings.trusted-users = cfg.managedUsers;

  networking.hosts = config.inventory.networks.home.hosts;
}
