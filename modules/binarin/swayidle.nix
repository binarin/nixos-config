{ self, ... }:
{
  flake.nixosModules.swayidle =
    { ... }:
    {
      key = "nixos-config.modules.nixos.swayidle";

      security.pam.services.swaylock = { };
    };

  flake.homeModules.swayidle =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.home.swayidle";

      imports = [
        self.homeModules.stylix
      ];

      options = {
        services.swayidle.binarin = {
          isLaptop = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether this is a laptop setup (enables shorter timeouts and suspend)";
          };

          brightness = {
            timeout = lib.mkOption {
              type = lib.types.int;
              description = "Brightness dim timeout in seconds";
            };
          };

          lock = {
            timeout = lib.mkOption {
              type = lib.types.int;
              description = "Lock session timeout in seconds";
            };
          };

          post-lock = {
            timeout = lib.mkOption {
              type = lib.types.int;
              description = "Post-lock action timeout in seconds";
            };

            command = lib.mkOption {
              type = lib.types.str;
              description = "Command to run after lock timeout";
            };
          };
        };
      };

      config =
        let
          cfg = config.services.swayidle.binarin;

          # Define preset defaults based on isLaptop
          laptopDefaults = {
            brightness.timeout = 60;
            lock.timeout = 90;
            post-lock.timeout = 120;
            post-lock.command = "${lib.getExe' pkgs.systemd "systemctl"} suspend";
          };

          desktopDefaults = {
            brightness.timeout = 180;
            lock.timeout = 300;
            post-lock.timeout = 330;
            post-lock.command = "${lib.getExe pkgs.niri} msg action power-off-monitors";
          };

          defaults = if cfg.isLaptop then laptopDefaults else desktopDefaults;
        in
        {
          # Apply computed defaults using lib.mkDefault (allows user overrides)
          services.swayidle.binarin.brightness.timeout = lib.mkDefault defaults.brightness.timeout;
          services.swayidle.binarin.lock.timeout = lib.mkDefault defaults.lock.timeout;
          services.swayidle.binarin.post-lock.timeout = lib.mkDefault defaults.post-lock.timeout;
          services.swayidle.binarin.post-lock.command = lib.mkDefault defaults.post-lock.command;

          stylix.targets.swaylock.enable = true;
          programs.swaylock.enable = true;

          services.swayidle = {
            enable = true;
            events = [
              {
                event = "before-sleep";
                command = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
              }
              {
                event = "after-resume";
                command = "${lib.getExe pkgs.niri} msg action power-on-monitors";
              }
              {
                event = "lock";
                command = "${lib.getExe pkgs.swaylock} -fF";
              }
            ];
            timeouts = [
              {
                timeout = cfg.brightness.timeout;
                command = "${lib.getExe pkgs.brightnessctl} -s s 10%";
                resumeCommand = "${lib.getExe pkgs.brightnessctl} -r";
              }
              {
                timeout = cfg.lock.timeout;
                command = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
              }
              {
                timeout = cfg.post-lock.timeout;
                command = cfg.post-lock.command;
              }
            ];
          };
        };
    };

}
