{...}: {
  flake.nixosModules.binarin-nix-dev = {config, pkgs, ...}: {
    environment.systemPackages = with pkgs; [
      nix-du
      nix-search-tv
    ];
    programs.nh = {
      enable = true;
      flake = "${config.users.users.binarin.home}/personal-workspace/nixos-config";
    };
    services.lorri.enable = true;
  };
  flake.homeModules.binarin-nix-dev = {pkgs, config, ...}: {
    programs.nix-search-tv.enable = true;
    home.packages = [
      (pkgs.writeShellScriptBin "ns" (config.lib.self.read "nix-search-tv.sh"))
    ];
  };
}
