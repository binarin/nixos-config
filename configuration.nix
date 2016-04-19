# -*- nix -*-
{ config, lib, pkgs, ... }:

{
  networking.hostName = "demandred";
  networking.hostId = "6e6740ba";

  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ./hardware/intel.nix
    ./partitions/uefi.nix
    ./partitions/unencrypted.nix

    ./roles/workstation.nix
    ./roles/openvpn-client.nix
  ];
}

 # Device: 
          #./devices/chromebook.nix
        #./devices/desktop.nix
        #./devices/homeserver.nix
        #./devices/netbook.nix
        #./devices/vultr-768.nix
      # Roles:
        #./roles/build-client.nix
        #./roles/common.nix
        #./roles/java-development.nix
        #./roles/steam-machine.nix
        #./roles/vultr-vps.nix
        #./roles/workstation.nix
        #./roles/workstation-extra.nix
