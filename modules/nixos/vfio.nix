{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.virtualization.vfio;
in
{
  options = {
    virtualization.vfio = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable PCI-e passthrough for some devices
        '';
      };
      devices = mkOption {
        type = with types; nullOr (listOf str);
        default = null;
        description = ''
          List of devices (like 0000:01:00.0) that should be ignored by host
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    boot.initrd.availableKernelModules = [ "vfio-pci" ];

    boot.initrd.preDeviceCommands = mkIf (cfg.devices != null) ''
      DEVS="${builtins.concatStringsSep " " cfg.devices}"
      for DEV in $DEVS; do
        echo "vfio-pci" > /sys/bus/pci/devices/$DEV/driver_override
      done
      modprobe -i vfio-pci
    '';
  };
}
