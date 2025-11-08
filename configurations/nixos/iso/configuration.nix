{
  flake,
  lib,
  pkgs,
  config,
  ...
}:
let
  inherit (flake) inputs;
in
{
  imports = [
    "${flake.inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
    "${flake.inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
  ];

  config = {
    boot = {
      supportedFilesystems = lib.mkForce [
        "btrfs"
        "reiserfs"
        "vfat"
        "f2fs"
        "xfs"
        "ntfs"
        "cifs"
        "zfs"
        "exfat"
      ];
    };

    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

    services.tailscale.enable = true;

    environment.systemPackages = with pkgs; [
      inputs.disko.packages."${config.nixpkgs.hostPlatform.system}".disko
    ];

    users.users.nixos.password = "nixos";
    users.users.nixos.initialHashedPassword = lib.mkForce null;

    console.useLargeFonts = true;

    systemd = {
      targets = {
        sleep.enable = false;
        suspend.enable = false;
        hibernate.enable = false;
        hybrid-sleep.enable = false;
      };
    };
  };
}
