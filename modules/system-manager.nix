{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    system-manager.url = "github:numtide/system-manager";
    system-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.systemConfigs.murmur = inputs.system-manager.lib.makeSystemConfig {
    overlays = self.lib.murmurOverlays;
    modules = [
      { nixpkgs.hostPlatform = "x86_64-linux"; }
      self.systemModules.bubuntu
      {
        boot.kernel.sysctl = {
          "kernel.unprivileged_userns_clone" = 1;
          "kernel.apparmor_restrict_unprivileged_userns" = 0;
        };
      }
    ];
  };
}
