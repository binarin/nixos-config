# Before https://github.com/NixOS/nixpkgs/pull/51457 is merged
self: super: {
  linuxPackages_4_14 = super.linuxPackages_4_14.extend(pSelf: pSuper: {
      legacy_340 = pSuper.legacy_340.overrideAttrs(oldAttrs: {
        patches = [ ../nixpkgs/pkgs/os-specific/linux/nvidia-x11/vm_operations_struct-fault.patch ];
      });
  });
}
