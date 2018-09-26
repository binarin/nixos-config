{config, pkgs, lib, ...}:

let
  machines = import ../nixops/personal-hosts.nix;
in {
  nix.sshServe.enable = true;
  nix.sshServe.keys = (lib.attrValues machines.balthamel.sshKeys);

  services.nix-serve = {
    enable = true;
    secretKeyFile = "/root/.nix-store-signing-key";
  };

  networking.firewall.allowedTCPPorts = [5000];
}
