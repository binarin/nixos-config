{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.v4l-play = pkgs.writeShellApplication {
        name = "v4l-play";
        runtimeInputs = with pkgs; [
          v4l-utils
          fzf
          vlc
          gawk
        ];
        text = ''
          device=$(v4l2-ctl --list-devices | awk '/^[^\t]/{name=$0;next} /\/dev\/video[0-9]/{if(!s[name]++){gsub(/\t/,"");print name" "$0}}' | fzf | awk '{print $NF}')

          if [[ -z "$device" ]]; then
            echo "No device selected"
            exit 1
          fi

          if ! vlc "v4l2://$device"; then
            vlc "v4l2://$device"
          fi
        '';
      };
    };
}
