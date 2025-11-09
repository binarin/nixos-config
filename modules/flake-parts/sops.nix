{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixosSharedModules = [ self.nixosModules.sops ];

  flake.nixosModules.sops =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.sops";

      imports = [ inputs.sops-nix.nixosModules.sops ];

      config = {
        # should be stringified path
        sops.defaultSopsFile = "${config.lib.self.file' "secrets/${config.inventoryHostName}/secrets.yaml"}";

        sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

        home-manager.sharedModules = [ self.homeModules.sops ];
      };
    };

  flake.homeModules.sops =
    { config, ... }:
    {
      key = "nixos-config.modules.home.sops";

      imports = [ inputs.sops-nix.homeManagerModules.sops ];

      config = {
        sops = {
          age.keyFile = "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
          defaultSopsFile = config.lib.self.optionalFile' "secrets/${config.inventoryHostName}/user-${config.home.username}.yaml";
        };
      };
    };
}
