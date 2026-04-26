{ self, ... }:
{

  flake.nixosModules.binarin-podman =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.binarin-podman";

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

      environment.etc."distrobox/distrobox.conf".text =
        let
          roVolumes = [
            "/nix/store"
          ]
          ++ (lib.optionals config.impermanence.enable [
            "/persist"
            "/local"
          ]);
          volumesStr =
            with lib;
            pipe roVolumes [
              (map (v: "${v}:${v}:ro"))
              (concatStringsSep " ")
            ];
        in
        ''
          container_additional_volumes="${volumesStr}"
        '';

      home-manager.users.binarin = self.homeModules.binarin-podman;
    };

  flake.homeModules.binarin-podman =
    { ... }:
    {
      key = "nixos-config.modules.home.binarin-podman";

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
