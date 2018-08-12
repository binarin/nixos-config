{config, pkgs, ...}:

{
  services.openvpn = {
    servers = {
      tcp-to-naberius = import ../vpn/tcp-to-naberius.nix { inherit config; inherit pkgs; };
      udp-to-naberius = import ../vpn/udp-to-naberius.nix { inherit config; inherit pkgs; };
      udp-to-amon = import ../vpn/udp-to-amon.nix { inherit config; inherit pkgs; };
      udp-to-mirantis = import ../vpn/udp-to-mirantis.nix { inherit config; inherit pkgs; };
      udp-to-airvpn = pkgs.lib.mkIf (config.networking.hostName == "ishamael")
                                      (import ../vpn/udp-to-airvpn.nix { inherit config; });
      udp-to-subscriptions = import ../vpn/udp-to-subscriptions-aws.nix { inherit config; inherit pkgs; };
    };
  };
}
