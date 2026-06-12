{ self, ... }:
{
  flake.systemModules.nix-path =
    { lib, pkgs, config, ... }:
    let
      cfg = config.bubuntu.nix.envPath;
      nixBin = "/nix/var/nix/profiles/default/bin";
    in
    {
      options.bubuntu.nix.envPath = {
        enable = lib.mkEnableOption "adding Nix to system PATH (/etc/environment and sudoers)" // {
          default = true;
        };
      };

      config = lib.mkIf cfg.enable {
        environment.etc."environment.d/50-nix.conf".text = ''
          PATH=${nixBin}:''${PATH}
        '';

        environment.etc."sudoers.d/nix-path" = {
          text = ''
            Defaults secure_path="${nixBin}:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin"
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

            if [ ! -f "$env_file" ]; then
              echo "PATH=\"$nix_bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\"" > "$env_file"
              exit 0
            fi

            if grep -q "$nix_bin" "$env_file"; then
              exit 0
            fi

            if grep -q "^PATH=" "$env_file"; then
              ${pkgs.gnused}/bin/sed -i "s|^PATH=\"|PATH=\"$nix_bin:|" "$env_file"
            else
              echo "PATH=\"$nix_bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\"" >> "$env_file"
            fi
          '';
        };
      };
    };
}
