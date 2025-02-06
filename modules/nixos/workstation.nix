{flake, lib, config, pkgs, ...}:
{
  config = lib.mkIf config.hostConfig.feature.workstation {
    environment.systemPackages = with pkgs; [
      sddm-astronaut
      bleeding.sddm-chili-theme
    ];

    services.desktopManager.plasma6.enable = true;
    services.xserver.enable = true;
    services.displayManager.sddm = {
      enable = true;
      theme = "sddm-astronaut-theme";
      extraPackages = with pkgs; [
      ];
    };
  };
}
