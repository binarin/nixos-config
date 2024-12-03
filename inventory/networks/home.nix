{
  info = {
    prefix = 24;
    network = "192.168.2.0";
    gateway = "192.168.2.1";
    dns = [
      "192.168.2.46"
      "192.168.2.53"
    ];
    domain = "home.binarin.info";
  };

  ipam = {
    "192.168.2.2" = "monitor";
    "192.168.2.3" = "forgejo";
    "192.168.2.4" = "lanfear";
    "192.168.2.5" = "nix-cache";
    "192.168.2.6" = "ishamael";
    "192.168.2.26" = "valak"; # [ "valak" "primary" ]
    "192.168.2.79" = "media";
  };
}
