{ inputs, ... }:
{
  imports = [ inputs.flake-parts.flakeModules.modules ];

  flake.modules = { };

  systems = [
    "x86_64-linux"
    "aarch64-linux"
  ];
}
