{config, pkgs, ...}:

{
  services.openvpn = {
    servers = {
      udp-to-naberius = pkgs.lib.mkIf (config.networking.hostName == "demandred") (import ../vpn/udp-to-naberius.nix { inherit config; });
    };
  };
}
