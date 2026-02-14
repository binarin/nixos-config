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
          brightness = {
            ac = {
              timeout = lib.mkOption {
                type = lib.types.int;
                default = 180;
                description = "Brightness dim timeout in seconds when on AC power";
              };
            };
            battery = {
              timeout = lib.mkOption {
                type = lib.types.int;
                default = 60;
                description = "Brightness dim timeout in seconds when on battery";
              };
            };
          };

          lock = {
            ac = {
              timeout = lib.mkOption {
                type = lib.types.int;
                default = 300;
                description = "Lock session timeout in seconds when on AC power";
              };
            };
            battery = {
              timeout = lib.mkOption {
                type = lib.types.int;
                default = 180;
                description = "Lock session timeout in seconds when on battery";
              };
            };
          };

          post-lock = {
            ac = {
              timeout = lib.mkOption {
                type = lib.types.int;
                default = 330;
                description = "Post-lock timeout in seconds when on AC power (powers off monitors)";
              };
            };
            battery = {
              timeout = lib.mkOption {
                type = lib.types.int;
                default = 200;
                description = "Post-lock timeout in seconds when on battery (suspends system)";
              };
            };
          };
        };
      };

      config =
        let
          cfg = config.services.swayidle.binarin;

          # Wrapper that handles both DDC monitors (via ddcutil) and laptop displays
          brightnessctl-wrapper = self.packages.${pkgs.system}.brightnessctl-wrapper;

          # Helper to create a wrapper script that runs a command only on AC power
          onAcPower =
            name: command:
            pkgs.writeShellApplication {
              name = "on-ac-power-${name}";
              runtimeInputs = [ pkgs.systemd ];
              text = ''
                # systemd-ac-power exits with 0 if on AC power, non-zero if on battery
                if systemd-ac-power; then
                  ${command}
                fi
              '';
            };

          # Helper to create a wrapper script that runs a command only on battery power
          onBatteryPower =
            name: command:
            pkgs.writeShellApplication {
              name = "on-battery-power-${name}";
              runtimeInputs = [ pkgs.systemd ];
              text = ''
                # systemd-ac-power exits with 0 if on AC power, non-zero if on battery
                if ! systemd-ac-power; then
                  ${command}
                fi
              '';
            };

          # Wrapped commands for brightness control (using wrapper for DDC monitor support)
          dimBrightnessAc = onAcPower "br-10" "${lib.getExe brightnessctl-wrapper} -s s 10%";
          dimBrightnessBattery = onBatteryPower "br-10" "${lib.getExe brightnessctl-wrapper} -s s 10%";

          # Wrapped commands for lock
          lockAc = onAcPower "lock-session" "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
          lockBattery = onBatteryPower "lock-session" "${lib.getExe' pkgs.systemd "loginctl"} lock-session";

          # Wrapped commands for post-lock actions
          postLockAc = onAcPower "niri-dpms-off" "${lib.getExe pkgs.niri} msg action power-off-monitors";
          postLockBattery = onBatteryPower "suspend" "${lib.getExe' pkgs.systemd "systemctl"} suspend";
        in
        {

          stylix.targets.swaylock.enable = true;
          programs.swaylock.enable = true;

          services.swayidle = {
            enable = true;
            extraArgs = [
              "-w"
              "-d"
            ];
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
              # Brightness dimming - AC power
              {
                timeout = cfg.brightness.ac.timeout;
                command = "${lib.getExe dimBrightnessAc}";
                resumeCommand = "${lib.getExe brightnessctl-wrapper} -r";
              }
              # Brightness dimming - Battery power
              {
                timeout = cfg.brightness.battery.timeout;
                command = "${lib.getExe dimBrightnessBattery}";
                resumeCommand = "${lib.getExe brightnessctl-wrapper} -r";
              }
              # Lock session - AC power
              {
                timeout = cfg.lock.ac.timeout;
                command = "${lib.getExe lockAc}";
              }
              # Lock session - Battery power
              {
                timeout = cfg.lock.battery.timeout;
                command = "${lib.getExe lockBattery}";
              }
              # Post-lock action - AC power
              {
                timeout = cfg.post-lock.ac.timeout;
                command = "${lib.getExe postLockAc}";
              }
              # Post-lock action - Battery power
              {
                timeout = cfg.post-lock.battery.timeout;
                command = "${lib.getExe postLockBattery}";
              }
            ];
          };
        };
    };

}
