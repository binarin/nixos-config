{
  self,
  inputs,
  config,
  ...
}:
let
  selfLib = self.lib.self;
in
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
      ];

      config = {
        # should be stringified path
        sops.defaultSopsFile = "${selfLib.file' "secrets/${config.networking.hostName}/secrets.yaml"}";

        sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      };
    };

  flake.homeModules.sops =
    { config, osConfig, ... }:
    {
      key = "nixos-config.modules.home.sops";

      imports = [
        inputs.sops-nix.homeManagerModules.sops
      ];

      config = {
        sops = {
          # Use the sops-nix decrypted user age key from NixOS module
          # This is decrypted using the SSH host key, so no need to pre-provision the age key
          age.keyFile =
            let
              # Check if the NixOS sops secret is defined for this user's age key
              sopsSecretPath = osConfig.sops.secrets.user-binarin-age.path or null;
            in
            if sopsSecretPath != null then
              sopsSecretPath
            else if osConfig.impermanence.enable then
              "/persist/${config.home.homeDirectory}/.config/age/nixos-config-keys.txt"
            else
              "${config.home.homeDirectory}/.config/age/nixos-config-keys.txt";
          defaultSopsFile = selfLib.optionalFile' "secrets/${osConfig.networking.hostName}/user-${config.home.username}.yaml";
        };
      };
    };
}
