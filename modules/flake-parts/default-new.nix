{self, ...}: {
  flake.nixosModules.default-new = {config, ...}: {
    key = "nixos-config.default-new";
    imports = (with self.nixosModules; [
      sshd
    ]) ++ (with self.sharedModules; [
      public-keys
    ]);
  };
}
