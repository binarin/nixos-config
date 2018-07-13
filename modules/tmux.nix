{lib, pkgs, ...}:

with lib;

let
  plugins = with pkgs.bleeding.tmuxPlugins; [
    continuum
    copycat
    open
    pain-control
    resurrect
    sensible
    sessionist
    yank
  ];
  tmuxConf = ''
    set -g prefix C-o
    # tmux-sensible will bind those
    unbind-key C-o
    unbind-key o

    set-window-option -g window-status-current-bg red
    set-option allow-rename off

    ${lib.concatStrings (map (x: "run-shell ${x.rtp}\n") plugins)}
  '';
in {
  environment = {
    etc."tmux.conf".text = tmuxConf;
    systemPackages = [ pkgs.tmux ];
  };
}
