{ inputs, ... }:
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
      environment.systemPackages = with pkgs; [
        nix-du
        nix-search-tv
      ];

      programs.nh = {
        enable = true;
        flake = "${config.users.users.binarin.home}/personal-workspace/nixos-config";
      };

      programs.direnv.enableZshIntegration = lib.mkForce false;
      programs.direnv.enableBashIntegration = lib.mkForce false;
    };

  flake.homeModules.binarin-nix-dev =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    {
      imports = [
        inputs.direnv-instant.homeModules.direnv-instant
      ];
      programs.direnv-instant.enable = true;

      programs.nix-search-tv.enable = true;
      home.packages = [
        (pkgs.writeShellScriptBin "ns" (config.lib.self.read "nix-search-tv.sh"))
      ];
    };
}
