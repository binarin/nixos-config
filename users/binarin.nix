{ config, pkgs, lib, ... }:
{
  config = {
    users.extraGroups = {
      binarin = {
        gid = 1000;
      };
    };

    users.extraUsers = {
      binarin = {
        description = "Alexey Lebedeff";
        uid = 1000;
        isNormalUser = true;
        group = "binarin";
        shell = "/run/current-system/sw/bin/zsh";
        extraGroups = [
          "users"
          "networkmanager"
          "docker"
          "libvirtd"
          "wheel"
          "dialout"
          "vboxusers"
          "wireshark"
          "transmission"
          "lxd"
          "video"
          "i2c"
        ];
      };
    };

    programs.zsh.enable = true;
  };
}
