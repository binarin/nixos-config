{
  self,
  config,
  lib,
  inputs,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.pi-box = {
    hostname = "pi-box";
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.pi-box;
    };
  };

  clan.inventory.machines.pi-box = {
    deploy.targetHost =
      if flakeConfig.inventory.ipAllocation ? pi-box then
        flakeConfig.inventory.ipAllocation.pi-box.home.primary.address
      else
        "0.0.0.0";
  };

  clan.machines.pi-box = {
    imports = [
      self.nixosModules.pi-box-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
    #    nixpkgs.hostPlatform = "x86_64-linux";
  };

  flake.nixosModules.pi-box-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.pi-box-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        self.nixosModules.binarin-workstation
      ];

      impermanence.enable = true;

      proxmoxLXC = {
        cores = 8;
        memory = 16384;
      };

      nixos-config.export-metrics.enable = false;
      home-manager.users.binarin = self.homeModules.pi-box-binarin;

      clan.core.vars.generators.forgejo = {
        prompts.api-key.description = "forgejo api key";
        files.api-key = { };
        script = ''
          cat $prompts/api-key > $out/api-key
        '';
      };

      clan.core.vars.generators.binarin-git-credentials = {
        dependencies = [ "forgejo" ];
        files.credentials = {
          owner = "binarin";
          group = "binarin";
          mode = "0600";
        };
        script = ''
          api_key="$(cat $in/forgejo/api-key)"
          cat <<EOF > $out/credentials
          https://pi-box:$api_key@forgejo.lynx-lizard.ts.net
          EOF
        '';
      };
    };

  flake.homeModules.pi-box-binarin =
    {
      osConfig,
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.pi-box-binarin";

      home.sessionVariables.FJ_FALLBACK_HOST = "https://forgejo.lynx-lizard.ts.net";

      programs.git.settings = {
        credential.helper = "store --file ${osConfig.clan.core.vars.generators.binarin-git-credentials.files.credentials.path}";

      };

    };

}
