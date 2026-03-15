{
  lib,
  inputs,
  self,
  ...
}:
{
  flake-file.inputs.clan-core = {
    # url = "https://git.clan.lol/clan/clan-core/archive/25.11.tar.gz";
    # url = "git+file:/home/binarin/personal-workspace/nix/clan-core";
    url = "git+https://forgejo.lynx-lizard.ts.net/binarin/clan-core?ref=25.11";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.disko.follows = "disko";
    inputs.flake-parts.follows = "flake-parts";
    inputs.sops-nix.follows = "sops-nix";
    inputs.treefmt-nix.follows = "treefmt-nix";
  };

  imports = [
    inputs.clan-core.flakeModules.default
  ];

  clan = {
    meta.name = "binarin-nixos-config";
    meta.domain = "clan.binarin.info";
  };

  flake.nixosModuels.clan-baseline =
    { ... }:
    {
      key = "nixos-config.modules.nixos.clan-baseline";
      imports = [
        self.nixosModules.clan-hostId
      ];
    };

  flake.nixosModules.clan-hostId =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.clan-hostId";
      clan.core.vars.generators.hostId = {
        files.hostId.secret = false;
        runtimeInputs = with pkgs; [
          openssl
        ];
        script = ''
          openssl rand -hex 4 > $out/hostId
        '';
      };
    };
}
