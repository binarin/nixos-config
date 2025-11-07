{self, ...}: {
  flake.nixosModules.default-new = {config, ...}: {
    key = "nixos-config.default-new";
    imports = [
      self.nixosModules.sshd
      self.nixosModules.tpm2-ssh
      self.nixosModules.nix
      self.sharedModules.public-keys
    ];
  };
}
