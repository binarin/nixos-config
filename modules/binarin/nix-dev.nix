{...}: {
  flake.nixosModules.binarin-nix-dev = {config, pkgs, ...}: {
    environment.systemPackages = with pkgs; [
      nix-du
    ];
    programs.nh = {
      enable = true;
      flake = "${config.users.users.binarin.home}/personal-workspace/nixos-config";
    };
    services.lorri.enable = true;
  };
}
