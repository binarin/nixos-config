{
  flake,
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;
in {
  config = {
    # tailscale can't use exit nodes otherwise
    # citing https://github.com/tailscale/tailscale/issues/4432#issuecomment-1112819111:
    #  Blindly applying strict RPF to all traffic just doesn't work
    #  any more in modern network environments, especially desktops
    #  which usually have multiple active interfaces at once and
    #  complex things like Tailscale going on.
    networking.firewall.checkReversePath = "loose";
  };
}
