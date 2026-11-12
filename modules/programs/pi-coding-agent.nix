{
  self,
  ...
}:
{
  flake.homeModules.pi-coding-agent =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.pi-coding-agent";
      imports = [ self.homeModules.agent-skills ];
      home.packages = [ pkgs.pi-with-extensions ];
    };
}
