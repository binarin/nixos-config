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

  flake.nixosModules.sops =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.sops";

      imports = [
        inputs.sops-nix.nixosModules.sops
        self.modules.generic.flake-files
      ];

      config = {
        # should be stringified path
        sops.defaultSopsFile = "${config.lib.self.file' "secrets/${config.networking.hostName}/secrets.yaml"}";

        sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      };
    };

  flake.homeModules.sops =
    { config, ... }:
    {
      key = "nixos-config.modules.home.sops";

      imports = [
        inputs.sops-nix.homeManagerModules.sops
        self.modules.generic.flake-files
      ];

      config = {
        sops = {
          age.keyFile = "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
          defaultSopsFile = config.lib.self.optionalFile' "secrets/${config.networking.hostName}/user-${config.home.username}.yaml";
        };
      };
    };
}
