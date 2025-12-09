{ ... }:
{
  flake.homeModules.insync =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.insync";

      home.packages = with pkgs; [
        insync
      ];

      xdg.configFile."autostart/insync.desktop".text = ''
        [Desktop Entry]
        Version=1.0
        Type=Application
        Name=Insync
        GenericName=Insync
        Comment=Launch Insync
        Icon=insync
        Categories=Network;
        Exec=insync start --no-daemon
        TryExec=insync
        Terminal=false
        X-GNOME-Autostart-Delay=3
      '';

      impermanence.persist-directories = [
        "OneDrive"
        ".config/Insync"
        ".local/share/Insync"
      ];

      impermanence.local-directories = [
        ".cache/Insync"
      ];
    };
}
