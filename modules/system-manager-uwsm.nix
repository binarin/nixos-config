{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.uwsm =
    { lib, pkgs, config, ... }:
    let
      cfg = config.programs.uwsm;
      uwsmPkg = cfg.package;

      uwsmUserUnits = builtins.readDir "${uwsmPkg}/lib/systemd/user";

      userUnitEtcEntries = lib.mapAttrs' (
        name: _:
        lib.nameValuePair "systemd/user/${name}" {
          source = "${uwsmPkg}/lib/systemd/user/${name}";
        }
      ) uwsmUserUnits;

      userPresetEtcEntries = {
        "systemd/user-preset/80-fumon.preset" = {
          source = "${uwsmPkg}/lib/systemd/user-preset/80-fumon.preset";
        };
      };

      mk_uwsm_desktop_entry =
        name: value:
        let
          wrapper = pkgs.writeShellScript "uwsm-${name}" ''
            export UWSM_BIN="${lib.getExe uwsmPkg}"
            ${value.preExec}
            exec ${if value.execCommand != null then value.execCommand else "$UWSM_BIN start -N ${name} -D ${name} -C ${name} -e -- ${value.binPath} ${lib.escapeShellArgs value.extraArgs}"}
          '';
          desktop = pkgs.writeTextFile {
            name = "${name}-uwsm-desktop";
            text = ''
              [Desktop Entry]
              Name=${value.prettyName} (UWSM)
              Comment=${value.comment or "${value.prettyName} compositor managed by UWSM"}
              Exec=${wrapper}
              TryExec=${lib.getExe uwsmPkg}
              DesktopNames=${name}
              Type=Application
            '';
            destination = "/share/wayland-sessions/${name}-uwsm.desktop";
          };
        in
        { inherit wrapper desktop; };

      compositorEntries = lib.mapAttrs mk_uwsm_desktop_entry cfg.waylandCompositors;
    in
    {
      options.programs.uwsm = {
        enable = lib.mkEnableOption "uwsm wayland session manager" // {
          default = true;
        };

        package = lib.mkPackageOption pkgs "uwsm" { };

        waylandCompositors = lib.mkOption {
          description = "UWSM-managed Wayland compositors with session desktop entries.";
          type = lib.types.attrsOf (
            lib.types.submodule {
              options = {
                prettyName = lib.mkOption {
                  type = lib.types.str;
                  description = "Display name for the session entry.";
                };
                comment = lib.mkOption {
                  type = lib.types.str;
                  default = "";
                  description = "Comment field for the desktop entry.";
                };
                binPath = lib.mkOption {
                  type = lib.types.str;
                  default = "";
                  description = "Path to the compositor binary (used in default execCommand).";
                };
                extraArgs = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  default = [ ];
                  description = "Extra arguments to pass to the compositor.";
                };
                preExec = lib.mkOption {
                  type = lib.types.lines;
                  default = "";
                  description = "Shell code to run before exec'ing uwsm (e.g. sourcing environment).";
                };
                execCommand = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = "Override the full exec command. If null, uses uwsm start with binPath.";
                };
              };
            }
          );
          default = { };
        };
      };

      config = lib.mkIf cfg.enable {
        environment.systemPackages = [ uwsmPkg ];

        environment.etc = userUnitEtcEntries // userPresetEtcEntries // {
          "profile.d/zz-prefer-nix-paths.sh".text = ''
            # Reorder PATH, XDG_DATA_DIRS, XDG_CONFIG_DIRS to prefer nix paths
            __reorder_paths() {
              local IFS=:
              local -a nix_paths=() other_paths=()
              for p in $1; do
                case "$p" in
                  /home/*|/nix/*) nix_paths+=("$p") ;;
                  *) other_paths+=("$p") ;;
                esac
              done
              local result=""
              for p in "''${nix_paths[@]}" "''${other_paths[@]}"; do
                [ -n "$p" ] && result="''${result:+$result:}$p"
              done
              echo "$result"
            }

            export PATH="$(__reorder_paths "$PATH")"
            export XDG_DATA_DIRS="$(__reorder_paths "''${XDG_DATA_DIRS:-/usr/local/share:/usr/share}")"
            export XDG_CONFIG_DIRS="$(__reorder_paths "''${XDG_CONFIG_DIRS:-/etc/xdg}")"
            unset -f __reorder_paths
          '';
        };

        systemd.tmpfiles.rules = lib.mkIf (cfg.waylandCompositors != { }) (
          lib.concatLists (lib.mapAttrsToList (name: v: [
            "d /usr/local/share/wayland-sessions 0755 root root -"
            "L+ /usr/local/share/wayland-sessions/${name}-uwsm.desktop - - - - ${compositorEntries.${name}.desktop}/share/wayland-sessions/${name}-uwsm.desktop"
          ]) cfg.waylandCompositors)
        );
      };
    };
}
