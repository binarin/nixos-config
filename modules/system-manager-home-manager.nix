{ self, inputs, ... }:
{
  flake.systemModules.home-manager =
    {
      config,
      lib,
      pkgs,
      utils,
      ...
    }:
    let
      cfg = config.home-manager;

      extendedLib = import "${inputs.home-manager}/modules/lib/stdlib-extended.nix" lib;

      hmModule = lib.types.submoduleWith {
        description = "Home Manager module";
        class = "homeManager";
        specialArgs = {
          lib = extendedLib;
          osConfig = config;
          systemManagerConfig = config;
          osClass = null;
          modulesPath = toString "${inputs.home-manager}/modules";
        } // cfg.extraSpecialArgs;

        modules = [
          ({ name, ... }: {
            imports =
              import "${inputs.home-manager}/modules/modules.nix" {
                inherit pkgs;
                lib = extendedLib;
                useNixpkgsModule = !cfg.useGlobalPkgs;
              }
              ++ cfg.sharedModules;

            config = {
              submoduleSupport.enable = true;

              home = {
                username = config.users.users.${name}.name;
                homeDirectory = config.users.users.${name}.home;
              } // lib.optionalAttrs (config.users.users.${name}.uid != null) {
                uid = config.users.users.${name}.uid;
              };

              nix.package = lib.mkDefault pkgs.nix;
            };
          })
        ];
      };
    in
    {
      options.home-manager = {
        users = lib.mkOption {
          type = lib.types.attrsOf hmModule;
          default = { };
          description = "Per-user Home Manager configuration.";
        };

        useGlobalPkgs = lib.mkEnableOption ''
          using the system configuration's `pkgs` argument in Home Manager.
          This disables the Home Manager options `nixpkgs.*`'';

        sharedModules = lib.mkOption {
          type = with lib.types; listOf raw;
          default = [ ];
          description = "Extra modules added to all users.";
        };

        extraSpecialArgs = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          description = "Extra `specialArgs` passed to Home Manager.";
        };

        backupFileExtension = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            On activation move existing files by appending the given
            file extension rather than exiting with an error.
          '';
        };
      };

      config = lib.mkIf (cfg.users != { }) {
        systemd.services = lib.mapAttrs' (
          _: usercfg:
          let
            inherit (usercfg.home) username homeDirectory activationPackage;
          in
          lib.nameValuePair "home-manager-${utils.escapeSystemdPath username}" {
            wantedBy = [ "system-manager.target" ];
            wants = [ "nix-daemon.socket" ];
            after = [ "nix-daemon.socket" ];

            unitConfig.RequiresMountsFor = homeDirectory;

            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = "yes";
              TimeoutStartSec = "5m";
              SyslogIdentifier = "hm-activate-${username}";
              User = username;
              ExecStart = let
                systemctl = "XDG_RUNTIME_DIR=\${XDG_RUNTIME_DIR:-/run/user/$UID} systemctl";
                sed = "${pkgs.gnused}/bin/sed";
                exportedSystemdVariables = lib.concatStringsSep "|" [
                  "DBUS_SESSION_BUS_ADDRESS"
                  "DISPLAY"
                  "WAYLAND_DISPLAY"
                  "XAUTHORITY"
                  "XDG_RUNTIME_DIR"
                ];
                setupEnv = pkgs.writeScript "hm-setup-env" ''
                  #! ${pkgs.runtimeShell} -el

                  # The activation script is run by a login shell to make sure
                  # that the user is given a sane environment.
                  # If the user is logged in, import variables from their current
                  # session environment.
                  eval "$(
                    ${systemctl} --user show-environment 2> /dev/null \
                    | ${sed} -En '/^(${exportedSystemdVariables})=/s/^/export /p'
                  )"

                  exec "$1/activate"
                '';
              in
              "${setupEnv} ${activationPackage}";
            };

            environment = lib.mkMerge [
              { QT_QPA_PLATFORM = "offscreen"; }
              (lib.mkIf (cfg.backupFileExtension != null) {
                HOME_MANAGER_BACKUP_EXT = cfg.backupFileExtension;
              })
            ];
          }
        ) cfg.users;
      };
    };
}
