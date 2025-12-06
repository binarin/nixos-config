{...}: {
  flake.nixosModules.binarin-podman = {pkgs, ...}: {
    virtualisation = {
      containers.enable = true;
      podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true; # Required for containers under podman-compose to be able to talk to each other.
      };
    };

    users.users.binarin.extraGroups = [
      "podman"
    ];

    environment.systemPackages = with pkgs; [
      distrobox
    ];
  };
}
