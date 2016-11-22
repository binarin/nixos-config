{
  network.description = "'naberius' server in Hetzner";
  naberius =
    {config, pkgs, ...}: {
      imports = [
        ../modules/server-packages.nix
        ../modules/static-blog.nix
      ];
      virtualisation.docker.enable = true;
      services.fail2ban.enable = true;
    };
}
