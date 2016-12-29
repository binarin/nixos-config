{config, pkgs, ...}:

{
  services.openvpn = {
    servers = {
      udp-to-naberius = pkgs.lib.mkIf (config.networking.hostName == "demandred" || config.networking.hostName == "ishamael")
                                      (import ../vpn/udp-to-naberius.nix { inherit config; });
      udp-to-mirantis = pkgs.lib.mkIf (config.networking.hostName == "ishamael")
                                      (import ../vpn/udp-to-mirantis.nix { inherit pkgs; });
      udp-to-airvpn = pkgs.lib.mkIf (config.networking.hostName == "ishamael")
                                      (import ../vpn/udp-to-airvpn.nix { inherit config; });
      udp-to-subscriptions = import ../vpn/udp-to-subscriptions-aws.nix { inherit config; inherit pkgs; };
    };
  };
}
