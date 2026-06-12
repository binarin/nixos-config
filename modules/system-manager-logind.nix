{ self, ... }:
{
  flake.systemModules.logind =
    { lib, pkgs, config, ... }:
    let
      cfg = config.services.logind;

      toOption =
        x:
        if x == true then
          "true"
        else if x == false then
          "false"
        else
          toString x;

      attrsToSection =
        as:
        lib.concatStrings (
          lib.concatLists (
            lib.mapAttrsToList (
              name: value:
              map (x: "${name}=${toOption x}\n") (if lib.isList value then value else [ value ])
            ) as
          )
        );

      settingsToSections =
        settings:
        lib.concatStringsSep "\n" (
          lib.mapAttrsToList (
            section_name: section_attrs: ''
              [${section_name}]
              ${attrsToSection section_attrs}
            ''
          ) settings
        );
    in
    {
      options.services.logind = {
        settings.Login = lib.mkOption {
          description = ''
            Settings for systemd-logind.
            See {manpage}`logind.conf(5)` for available options.
          '';
          type = lib.types.submodule {
            freeformType = lib.types.attrsOf (lib.types.oneOf [
              lib.types.bool
              lib.types.int
              lib.types.str
            ]);
          };
          default = { };
          example = {
            HandleLidSwitch = "ignore";
            HandleLidSwitchExternalPower = "ignore";
          };
        };

        lidSwitch = lib.mkOption {
          type = lib.types.str;
          default = "suspend";
          description = "What to do when the laptop lid is closed.";
        };

        lidSwitchExternalPower = lib.mkOption {
          type = lib.types.str;
          default = "lock";
          description = "What to do when the laptop lid is closed and connected to external power.";
        };

        lidSwitchDocked = lib.mkOption {
          type = lib.types.str;
          default = "ignore";
          description = "What to do when the laptop lid is closed and another monitor is connected.";
        };
      };

      config = {
        services.logind.settings.Login = {
          HandleLidSwitch = lib.mkDefault cfg.lidSwitch;
          HandleLidSwitchExternalPower = lib.mkDefault cfg.lidSwitchExternalPower;
          HandleLidSwitchDocked = lib.mkDefault cfg.lidSwitchDocked;
        };

        environment.etc."systemd/logind.conf.d/50-system-manager.conf" = {
          text = settingsToSections cfg.settings;
          replaceExisting = true;
        };
      };
    };
}
