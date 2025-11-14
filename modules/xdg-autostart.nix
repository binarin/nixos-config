{ ... }:
{
  flake.homeModules.xdg-autostart =
    { lib, config, ... }:
    let
      cfg = config.xdg.autostart.override;
      optsModule =
        with lib;
        with types;
        submodule {
          options = {
            hidden = mkOption {
              description = "No service will be generated if set to true (Hidden= entry)";
              type = nullOr bool;
              default = null;
            };
            x-systemd-skip = mkOption {
              description = "No service will be generated if set to true (X-systemd-skip= entry)";
              type = nullOr bool;
              default = null;
            };
            notShownIn = mkOption {
              description = "ExecCondition= using systemd-xdg-autostart-condition (NotShowIn= entry)";
              type = nullOr (listOf str);
              default = null;
            };
            onlyShownIn = mkOption {
              description = "ExecCondition= using systemd-xdg-autostart-condition (OnlyShowIn= entry)";
              type = nullOr (listOf str);
              default = null;
            };
          };
        };
    in
      {
        key = "nixos-config.modules.home.xdg-autostart";
        options = {
          xdg.autostart.override = lib.mkOption {
            type = with lib.types; attrsOf optsModule;
            default = { };
          };
        };
        config = lib.mkMerge [
          {
            assertions = lib.mapAttrsToList (
              name: options: {
                assertion = !(options.notShownIn != null && options.onlyShownIn != null);
                message = "xdg.autostart.override.${name}: Cannot specify both notShownIn and onlyShownIn for the same desktop entry";
              }
            ) cfg;

            xdg.configFile =
              with lib;
              flip mapAttrs' cfg (
                name: options:
                nameValuePair "autostart/${name}.desktop" {
                  text =
                    ''
                      [Desktop Entry]
                    ''
                    + optionalString (options.hidden != null) "Hidden=${if options.hidden then "true" else "false"}\n"
                    + optionalString (options.x-systemd-skip != null) "X-systemd-skip=${
                      if options.x-systemd-skip then "true" else "false"
                    }\n"
                    + optionalString (options.notShownIn != null) "NotShowIn=${concatStringsSep ";" options.notShownIn}\n"
                    + optionalString (options.onlyShownIn != null) "OnlyShowIn=${concatStringsSep ";" options.onlyShownIn}\n";
                }
              );
          }
        ];
      };
}
