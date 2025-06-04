{flake, lib, config, pkgs, ...}:
{
  config = lib.mkIf config.hostConfig.feature.workstation {
    environment.systemPackages = with pkgs; [
      sddm-astronaut
      sddm-chili-theme
    ];

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };

    services.blueman.enable = true;
    services.desktopManager.plasma6.enable = true;
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      theme = "sddm-astronaut-theme";
    };
    # services.xserver.enable = true;
  };
}
