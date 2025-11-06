{...}: {
  flake.nixosModules.impermanence-new = {lib, config, ...}: {
    key = "nixos-config.impermanenc-new";
    options = {
      impermanence.enable = lib.mkEnableOption "Enable impermanence";
    };
    config = lib.mkMerge [
      {
        impermanence.enable = lib.mkDefault config.hostConfig.feature.impermanence;
      }
      (lib.mkIf config.virtualisation.libvirtd.enable {
        assertions = [
          {
            assertion = config.fileSystems ? "/var/lib/libvirt";
            message = "/var/lib/libvirt should be persisted, either by impermanence or explicitely separately mounted";
          }
        ];
      })
    ];
  };
}
