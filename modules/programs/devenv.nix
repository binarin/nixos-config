{
  self,
  inputs,
  lib,
  ...
}:
{
  flake-file.inputs = {
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

  flake.overlays.devenv-upstream = final: prev: {
    devenv = inputs.devenv.packages."${final.stdenv.hostPlatform.system}".devenv;
  };

  flake.nixosModules.devenv =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.devenv";
      # nixpkgs.overlays = [
      #   self.overlays.devenv-upstream
      # ];
      environment.systemPackages = with pkgs; [
        devenv
      ];
    };

  flake.homeModules.devenv =
    {
      pkgs,
      lib,
      osConfig ? null,
      ...
    }:
    {
      key = "nixos-config.modules.home.devenv";
      config = lib.mkIf (osConfig == null) {
        # nixpkgs.overlays = [
        #   self.overlays.devenv-upstream
        # ];
        home.packages = [
          pkgs.devenv
        ];
      };
    };

}
