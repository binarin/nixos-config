{ self, config, ... }:
let
  selfLib = self.lib.self;
in
{
  flake.nixosModules.tailscale =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      hostname = config.networking.hostName;
      hasTailscaleAuth = builtins.pathExists "${self}/secrets/${hostname}/tailscale-auth";
    in
    {
      key = "nixos-config.modules.nixos.tailscale";

      config = lib.mkMerge [
        # Existing tailscale config
        {
          services.tailscale = {
            enable = true;

            # Works only if authKeyFile is set
            extraUpFlags = [
              "--hostname"
              "${config.networking.hostName}"
              "--accept-routes"
            ];

            extraSetFlags = [
              "--accept-routes"
            ];
            useRoutingFeatures = "both";
          };
        }

        # Auto-detect tailscale auth key from secrets
        (lib.mkIf hasTailscaleAuth {
          sops.secrets.tailscale-auth = {
            sopsFile = selfLib.file' "secrets/${hostname}/tailscale-auth";
            format = "binary";
          };
          services.tailscale.authKeyFile = config.sops.secrets.tailscale-auth.path;
        })
      ];
    };
}
