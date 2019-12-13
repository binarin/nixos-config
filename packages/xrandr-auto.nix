{config, pkgs, ...}:

{
  config = {
    nixpkgs.config.packageOverrides = super: {
      binarin-xrandr-auto = pkgs.writeScriptBin "xrandr-auto" ''
        #!${pkgs.bash}/bin/bash
        set -euxo pipefail
        export PATH=$PATH''${PATH:+:}${pkgs.gnugrep}/bin:${pkgs.xorg.xrandr}/bin:${pkgs.procps}/bin:${pkgs.findutils}/bin:${pkgs.coreutils}/bin:${pkgs.gawk}/bin

        mode="''${1:-}"

        LID_STATUS=$(cat /proc/acpi/button/lid/LID0/state | awk '{print $2}')

        HDMI_STATUS=
        HDMI_OUTPUT=
        if xrandr | grep -q 'HDMI-1 connected'; then
            HDMI_STATUS=connected
            HDMI_OUTPUT=HDMI-1
        fi
        if xrandr | grep -q 'HDMI1 connected'; then
            HDMI_STATUS=connected
            HDMI_OUTPUT=HDMI1
        fi

        DP1_1_STATUS=
        DP1_1_OUTPUT=
        if xrandr | grep -q 'DP-1-1 connected'; then
            DP1_1_STATUS=connected
            DP1_1_OUTPUT=DP-1-1
        fi
        if xrandr | grep -q 'DP1-1 connected'; then
            DP1_1_STATUS=connected
            DP1_1_OUTPUT=DP1-1
        fi

        DP1_2_STATUS=
        DP1_2_OUTPUT=
        if xrandr | grep -q 'DP-1-2 connected'; then
            DP1_2_STATUS=connected
            DP1_2_OUTPUT=DP-1-2
        fi
        if xrandr | grep -q 'DP1-2 connected'; then
            DP1_2_STATUS=connected
            DP1_2_OUTPUT=DP1-2
        fi

        DP1_3_STATUS=
        DP1_3_OUTPUT=
        if xrandr | grep -q 'DP-1-3 connected'; then
            DP1_3_STATUS=connected
            DP1_3_OUTPUT=DP-1-3
        fi
        if xrandr | grep -q 'DP1-3 connected'; then
            DP1_3_STATUS=connected
            DP1_3_OUTPUT=DP1-3
        fi

        EDP1_OUTPUT=eDP-1
        if xrandr | grep -q 'eDP1 connected'; then
          EDP1_OUTPUT=eDP1
        fi

        set +o pipefail
        WORK_DELL_COUNT=$(find /sys -name edid 2>/dev/null | grep -P 'card\d/card\d-DP-\d/edid' | xargs cat | grep -a 'DELL P2417H' | wc -l)
        set -o pipefail

        setup=
        if [[ connected == $DP1_1_STATUS && connected == $DP1_2_STATUS && 2 == $WORK_DELL_COUNT ]]; then
            setup=work
        elif [[ connected == $DP1_1_STATUS && "" == $DP1_2_STATUS ]]; then
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
                xrandr --output $DP1_1_OUTPUT --mode 1920x1080 --primary
                xrandr --output $DP1_2_OUTPUT --mode 1920x1080 --left-of $DP1_1_OUTPUT
                xrandr --output $EDP1_OUTPUT --mode 1920x1080 --right-of $DP1_1_OUTPUT
                xrandr --output $DP1_3_OUTPUT --off
                xrandr --output $HDMI_OUTPUT --off
                ;;
              home)
                xrandr --output $EDP1_OUTPUT --mode 1920x1080 --primary
                xrandr --output $DP1_1_OUTPUT --mode 1920x1080 --same-as $EDP1_OUTPUT
                xrandr --output $DP1_2_OUTPUT --off
                xrandr --output $DP1_3_OUTPUT --off
                xrandr --output $HDMI_OUTPUT --off
                ;;
              presentation)
                xrandr --output $EDP1_OUTPUT --mode 1920x1080
                xrandr --output $DP1_1_OUTPUT --off
                xrandr --output $DP1_2_OUTPUT --off
                xrandr --output $DP1_3_OUTPUT --off
                xrandr --output $HDMI_OUTPUT --mode 1920x1080 --same-as $EDP1_OUTPUT
                ;;
              standalone)
                xrandr --output $EDP1_OUTPUT --mode 1920x1080
                xrandr --output $HDMI_OUTPUT --off
                xrandr --output $DP1_1_OUTPUT --off
                xrandr --output $DP1_2_OUTPUT --off
                xrandr --output $DP1_3_OUTPUT --off
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
                  exit 1
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
