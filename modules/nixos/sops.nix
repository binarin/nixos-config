{ flake, config, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    flake.inputs.sops-nix.nixosModules.sops
  ];

  sops.defaultSopsFile = "${self}/secrets/${config.inventoryHostName}/secrets.yaml";
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
}
