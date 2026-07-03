{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.yum-packages =
    { lib, pkgs, config, ... }:
    let
      cfg = config.bentos.yum.packages;
    in
    {
      options.bentos.yum.packages = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Packages to ensure are installed via yum.";
        example = [ "python3-pip" "htop" ];
      };

      config = lib.mkIf (cfg != [ ]) {
        systemd.services.yum-ensure-packages = {
          wantedBy = [ "system-manager.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            installed=true
            for pkg in ${lib.escapeShellArgs cfg}; do
              if ! rpm -q "$pkg" >/dev/null 2>&1; then
                installed=false
                break
              fi
            done

            if [ "$installed" = "false" ]; then
              yum install -y ${lib.escapeShellArgs cfg}
            fi
          '';
        };
      };
    };
}
