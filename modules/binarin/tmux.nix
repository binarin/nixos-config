{ ... }: {
  flake.homeModules.binarin-tmux =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      programs.tmux = {
        enable = true;

        baseIndex = 1;
        clock24 = true;
        shortcut = "o";
        terminal = "tmux-256color";
        mouse = true;
        focusEvents = true;

        plugins = with pkgs.tmuxPlugins; [
          {
            plugin = extrakto;
          }
          {
            plugin = pain-control;
          }
          {
            plugin = sensible;
          }
        ];

        extraConfig = ''
          # even having 'sensible' plugin last is not enough to do these 2 binds
          bind-key C-o last-window
          bind-key o send-prefix

          set -g word-separators ' "=()[]'
          set -ag word-separators "'"  # '-a' for append, "'" should be quoted differently

          set -g allow-rename off
          set -g update-environment "DISPLAY KRB5CCNAME SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY SSH_AUTH_SOCK WAYLAND_DISPLAY NIRI_SOCKET"

          set-option -s set-clipboard on
          set-option -as terminal-overrides "xterm-256color:Ms=\\E]52;c%p1%.0s;%p2%s\\7" # only for mosh - https://github.com/mobile-shell/mosh/pull/1054

          set-option -s extended-keys on
          set-option -s extended-keys-format csi-u

          set -as terminal-features ",foot*:hyperlinks"
        '';
      };
    };
}
