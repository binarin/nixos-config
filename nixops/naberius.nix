{
  network.description = "'naberius' server in Hetzner";
  naberius =
    {config, pkgs, ...}: {
      imports = [
        ../modules/server-packages.nix
        ../modules/static-blog.nix
        ../modules/outgoing-email.nix
      ];
      virtualisation.docker.enable = true;
      virtualisation.libvirtd.enable = true;
      services.fail2ban.enable = true;
    };
}
