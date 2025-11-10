{ ... }:
{
  flake.nixosModules.tailscale =
    { lib, config, ... }:
    {
      key = "nixos-config.modules.nixos.tailscale";

      config = {
        services.tailscale = {
          enable = true;
          extraUpFlags = [
            "--hostname"
            "${config.networking.hostName}"
          ];
        };

        # tailscale can't use exit nodes otherwise
        # citing https://github.com/tailscale/tailscale/issues/4432#issuecomment-1112819111:
        #  Blindly applying strict RPF to all traffic just doesn't work
        #  any more in modern network environments, especially desktops
        #  which usually have multiple active interfaces at once and
        #  complex things like Tailscale going on.
        networking.firewall.checkReversePath = lib.mkForce "loose";
      };
    };
}
