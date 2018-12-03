# Before https://github.com/NixOS/nixpkgs/pull/51457 is merged
self: super: {
  linuxPackages_4_14 = super.linuxPackages_4_14.extend(pSelf: pSuper: {
      nvidia_x11_legacy340 = pSuper.nvidia_x11_legacy340.overrideAttrs(oldAttrs: {
        patches = [ ../nixpkgs/pkgs/os-specific/linux/nvidia-x11/vm_operations_struct-fault.patch ];
      });
  });
}
