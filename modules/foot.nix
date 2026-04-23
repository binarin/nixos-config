{
  ...
}:
{
  flake.homeModules.foot =
    {
      config,
      lib,
      osConfig,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.home.foot";

      config = lib.mkIf osConfig.services.graphical-desktop.enable {
        xdg.configFile."xdg-terminals.list".text = ''
          foot.desktop
          /execarg_default:org.codeberg.dnkl.foot.desktop:--
        '';

        home.packages = [
          (pkgs.writeShellApplication {
            name = "foot-unique-window";
            runtimeInputs = with pkgs; [
              niri
              jq
              gnugrep
              foot
            ];
            text = ''
              title="$1"

              find-and-focus-window() {
                  local title
                  title="$1"

                  if niri msg --json windows | jq -r '.[].title' | grep -q -F "$title"; then
                      local window_id
                      window_id="$(niri msg --json windows | jq -r ".[] | select(.title == \"$title\") | .id")"
                      niri msg action focus-window --id "$window_id"
                      return 0
                  else
                      return 1
                  fi
              }

              switch-or-run-foot() {
                  local title
                  title="''${1:?}"; shift

                  if find-and-focus-window "$title"; then
                      exit 0
                  else
                      exec uwsm app -a "sshmenu-$title" -- foot --title "$title" "$@"
                  fi
              }

              switch-or-run-foot "$@"
            '';
          })
        ];

        programs.foot = {
          enable = true;
          settings = {
            main = {
              font = with config.stylix.fonts; "${monospace.name}:size=${toString sizes.terminal}";
              locked-title = true;
              selection-target = "both";
            };
            url = {
              osc8-underline = "always";
            };
            colors = {
              background = "000000";
              regular0 = "000000";
              regular1 = "cd0000";
              regular2 = "00cd00";
              regular3 = "cdcd00";
              regular4 = "366080";
              regular5 = "cd00cd";
              regular6 = "00cdcd";
              regular7 = "faebd7";
              bright0 = "404040";
              bright1 = "ff0000";
              bright2 = "00ff00";
              bright3 = "ffff00";
              bright4 = "0000ff";
              bright5 = "ff00ff";
              bright6 = "00ffff";
              bright7 = "ffffff";
            };
          };
        };
      };
    };
}
