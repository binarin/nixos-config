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

          # Workaround for some hardware (like nanokvm capture card) that fails on first attempt
          start=$SECONDS
          if ! vlc "v4l2://$device"; then
            elapsed=$((SECONDS - start))
            if ((elapsed < 2)); then
              vlc "v4l2://$device"
            fi
          fi
        '';
      };
    };
}
