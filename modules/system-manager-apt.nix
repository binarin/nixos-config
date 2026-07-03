{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.apt-packages =
    { lib, pkgs, config, ... }:
    let
      cfg = config.bubuntu.apt.packages;
    in
    {
      options.bubuntu.apt.packages = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Packages to ensure are installed via apt.";
        example = [ "swaylock" "gdm3" ];
      };

      config = lib.mkIf (cfg != [ ]) {
        systemd.services.apt-ensure-packages = {
          wantedBy = [ "system-manager.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            export PATH="/usr/bin:/usr/sbin:/bin:/sbin:$PATH"
            installed=true
            for pkg in ${lib.escapeShellArgs cfg}; do
              if ! dpkg -s "$pkg" >/dev/null 2>&1; then
                installed=false
                break
              fi
            done

            if [ "$installed" = "false" ]; then
              apt-get update -qq
              apt-get install -y --no-install-recommends ${lib.escapeShellArgs cfg}
            fi
          '';
        };
      };
    };
}
