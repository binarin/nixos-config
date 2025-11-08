{
  config,
  ...
}:
{
  networking.hostName = config.inventoryHostName;
  networking.hostId = config.hostConfig.hostId;

  networking.hosts = config.inventory.networks.home.hosts;
}
