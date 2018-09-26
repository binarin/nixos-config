{config, pkgs, lib, ...}:

let
  machines = import ../nixops/personal-hosts.nix;
in {
  nix.sshServe.enable = true;
  nix.sshServe.keys = (lib.attrValues machines.balthamel.sshKeys);
}
