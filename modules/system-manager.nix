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
      (
        { nixosModulesPath, ... }:
        {
          disabledModules = [
            (nixosModulesPath + "/services/web-servers/nginx/")
            "${inputs.system-manager}/nix/modules/upstream/nixpkgs/nginx.nix"
          ];
        }
      )
    ];
  };
}
