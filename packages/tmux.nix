{lib, pkgs, ...}:

with lib;

let
  plugins = with pkgs.tmuxPlugins; [
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


    ${lib.concatStrings (map (x: "run-shell ${x.rtp}\n") plugins)}
  '';
in {
  environment = {
    etc."tmux.conf".text = tmuxConf;
    systemPackages = [ pkgs.tmux ];
  };
}
