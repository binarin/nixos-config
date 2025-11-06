{self, ...}: {
  flake.nixosModules.default-new = {config, ...}: {
    key = "nixos-config.default-new";
    imports = (with self.nixosModules; [
      sshd
      tpm2-ssh
      nix
    ]) ++ (with self.sharedModules; [
      public-keys
    ]);
  };
}
