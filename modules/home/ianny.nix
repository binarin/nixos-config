{flake, pkgs, lib, config, ...}:
{
  config = lib.mkIf config.hostConfig.feature.wayland {
    home.packages = [ pkgs.ianny ];
    xdg.configFile."io.github.zefr0x.ianny/config.toml".text = ''
    [timer]
    # Timer will stop and reset when you are idle for this amount of seconds.
    idle_timeout = 240
    # Active duration that activates a break.
    short_break_timeout = 1200
    long_break_timeout = 3840
    # Breaks duration.
    short_break_duration = 120
    long_break_duration = 240

    [notification]
    show_progress_bar = true
    # Minimum delay of updating the progress bar (lower than 1s may return an error).
    minimum_update_delay = 1
  '';
  };
}
