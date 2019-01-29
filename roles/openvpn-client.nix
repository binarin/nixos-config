{config, pkgs, ...}:

{
  services.openvpn = {
    servers = {
      tcp-to-naberius = import ../vpn/tcp-to-naberius.nix { inherit config; inherit pkgs; };
      tcp-to-lanfear = import ../vpn/tcp-to-lanfear.nix { inherit config; inherit pkgs; };
    };
  };
}
