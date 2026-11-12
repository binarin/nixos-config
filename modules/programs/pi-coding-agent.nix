{ ... }:
{
  flake.homeModules.pi-coding-agent =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.pi-coding-agent";
      home.packages = [ pkgs.pi-with-extensions ];
    };
}
