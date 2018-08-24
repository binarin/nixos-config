{config, pkgs, ...}:

{
  config = {
    nixpkgs.config.packageOverrides = super: {
      binarin-xrandr-auto = pkgs.writeScriptBin "xrandr-auto" ''
        #!${pkgs.bash}/bin/bash
        set -euxo pipefail
        export PATH=$PATH''${PATH:+:}${pkgs.gnugrep}/bin:${pkgs.xorg.xrandr}/bin:${pkgs.procps}/bin:${pkgs.findutils}/bin:${pkgs.coreutils}/bin

        mode="''${1:-}"

        HDMI_STATUS=
        if xrandr | grep -q 'HDMI-1 connected'; then
            HDMI_STATUS=connected
        fi
        DP1_1_STATUS=
        if xrandr | grep -q 'DP-1-1 connected'; then
            DP1_1_STATUS=connected
        fi
        DP1_2_STATUS=
        if xrandr | grep -q 'DP-1-2 connected'; then
            DP1_2_STATUS=connected
        fi
        DP1_3_STATUS=
        if xrandr | grep -q 'DP-1-3 connected'; then
            DP1_3_STATUS=connected
        fi

        set +o pipefail
        WORK_DELL_COUNT=$(find /sys -name edid 2>/dev/null | grep -P 'card\d/card\d-DP-\d/edid' | xargs cat | grep -a 'DELL P2417H' | wc -l)
        set -o pipefail

        setup=
        if [[ connected == $DP1_1_STATUS && connected == $DP1_2_STATUS && 2 == $WORK_DELL_COUNT ]]; then
            setup=work
        elif [[ connected == $DP1_1_STATUS && connected == $DP1_2_STATUS ]]; then
            setup=home
        elif [[ connected == $HDMI_STATUS ]]; then
            setup=presentation
        else
            setup=standalone
        fi

        case "$mode" in
          configure)
            case "$setup" in
              work)
                xrandr --output DP-1-1 --mode 1920x1080 --primary
                xrandr --output DP-1-2 --mode 1920x1080 --left-of DP-1-1
                xrandr --output eDP-1 --mode 1920x1080 --right-of DP-1-1
                xrandr --output DP-1-3 --off
                xrandr --output HDMI-1 --off
                ;;
              home)
                xrandr --output eDP-1 --mode 1920x1080
                xrandr --output DP-1-2 --mode 1920x1080 --above eDP-1
                xrandr --output DP-1-1 --mode 1920x1080 --primary --right-of DP-1-2
                xrandr --output DP-1-3 --off
                xrandr --output HDMI-1 --off
                ;;
              presentation)
                xrandr --output eDP-1 --mode 1920x1080
                xrandr --output DP-1-1 --off
                xrandr --output DP-1-2 --off
                xrandr --output DP-1-3 --off
                xrandr --output HDMI-1 --mode 1920x1080 --same-as eDP-1
                ;;
              standalone)
                xrandr --output eDP-1 --mode 1920x1080
                xrandr --output HDMI-1 --off
                xrandr --output DP-1-1 --off
                xrandr --output DP-1-2 --off
                xrandr --output DP-1-3 --off
                ;;
              esac
              xrandr
              pkill -f taffybar
              pkill -f compton
          ;;
          get-primary)
              case "$setup" in
                work)
                  exit 1
                  ;;
                home)
                  exit 2
                  ;;
                presentation)
                  exit 0
                  ;;
                standalone)
                  exit 0
                  ;;
              esac
          ;;
          *)
          echo "usage: $0 <configure|get-primary>"
          exit 0
          ;;
        esac
      '';
    };
  };
}
