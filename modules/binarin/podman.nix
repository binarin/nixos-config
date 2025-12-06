{self, ...}: {

  flake.nixosModules.binarin-podman = {pkgs, ...}: {
    imports = [
      self.nixosModules.impermanence
    ];

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

    home-manager.users.binarin = self.homeModules.binarin-podman;
  };

  flake.homeModules.binarin-podman = {...}: {
    imports = [
      self.homeModules.impermanence
    ];

    impermanence.persist-directories = [
      ".local/share/containers"
    ];

    impermanence.local-directories = [
      ".cache/containers"
    ];
  };
}
