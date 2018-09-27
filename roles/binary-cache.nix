{config, pkgs, lib, ...}:

let
  machines = import ../nixops/personal-hosts.nix;
in {
  nix.sshServe.enable = true;
  nix.sshServe.keys = (lib.attrValues machines.balthamel.sshKeys);

  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/lib/nix-serve/nix-store-secret";
  };

  users.users.nix-serve.extraGroups = [ "keys" ];

  networking.firewall.allowedTCPPorts = [5000];
}
