{ self, inputs, ... }:
{
  flake-file.inputs.direnv-instant.url = "github:Mic92/direnv-instant";
  flake.nixosModules.binarin-nix-dev =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.binarin-nix-dev";
      environment.systemPackages = with pkgs; [
        nix-search-tv
      ];

      programs.nh = {
        enable = true;
        flake = "${config.users.users.binarin.home}/personal-workspace/nixos-config";
      };

      programs.direnv.enableZshIntegration = lib.mkForce false;
      programs.direnv.enableBashIntegration = lib.mkForce false;

      # improve desktop responsiveness when updating the system
      nix.daemonCPUSchedPolicy = "idle";
    };

  flake.homeModules.binarin-nix-dev =
    {
      pkgs,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.home.binarin-nix-dev";
      imports = [
        inputs.direnv-instant.homeModules.direnv-instant
      ];
      programs.direnv-instant.enable = true;

      programs.nix-search-tv.enable = true;
      home.packages = [
        (pkgs.writeShellScriptBin "ns" (config.lib.self.read "nix-search-tv.sh"))
        self.packages.${pkgs.stdenv.hostPlatform.system}.pin-nixpkgs-to-system
      ];
    };
}
