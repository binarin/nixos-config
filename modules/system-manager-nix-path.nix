{ self, ... }:
{
  flake.systemModules.nix-path =
    { lib, pkgs, config, ... }:
    let
      cfg = config.bubuntu.nix.envPath;
      nixBin = "/nix/var/nix/profiles/default/bin";
      swBin = "/run/current-system/sw/bin";
    in
    {
      options.bubuntu.nix.envPath = {
        enable = lib.mkEnableOption "adding Nix to system PATH (/etc/environment and sudoers)" // {
          default = true;
        };
      };

      config = lib.mkIf cfg.enable {
        environment.etc."environment.d/50-nix.conf".text = ''
          PATH=''${HOME}/.local/bin:${nixBin}:${swBin}:''${PATH}
        '';

        environment.etc."sudoers.d/nix-path" = {
          text = ''
            Defaults secure_path="${swBin}:${nixBin}:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin"
          '';
          mode = "0440";
        };

        systemd.services.nix-env-path = {
          wantedBy = [ "system-manager.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            env_file="/etc/environment"
            nix_bin="${nixBin}"
            sw_bin="${swBin}"

            ensure_path_entry() {
              local file="$1" entry="$2"
              if grep -q "$entry" "$file" 2>/dev/null; then
                return 0
              fi
              sed -i "s|^PATH=\"|PATH=\"$entry:|" "$file"
            }

            if [ ! -f "$env_file" ]; then
              echo "PATH=\"$nix_bin:$sw_bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\"" > "$env_file"
            else
              if ! grep -q "^PATH=" "$env_file"; then
                echo "PATH=\"$nix_bin:$sw_bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\"" >> "$env_file"
              else
                ensure_path_entry "$env_file" "$nix_bin"
                ensure_path_entry "$env_file" "$sw_bin"
              fi
            fi
          '';
        };
      };
    };
}
