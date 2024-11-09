{flake, pkgs, lib, config, ...}:

let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.services.lnxlink;
  yaml = pkgs.formats.yaml { };

  passwordPlaceholder = "@MQTT_PASSWORD@";

  allAddons = cfg.package.meta.addons.allNames;

  moduleNameType = lib.types.enum allAddons;

  passwordFileType = with lib; mkOptionType {
    name = "passwordFile";
    description = "Path to a password file - outside of nix store";
    descriptionClass = "noun";
    check = x: !(isStorePath x);
    merge = lib.mergeEqualOption;
  };

  mqttModule = with lib; types.submodule {
    options = {
      clientId = mkOption {
        type = types.str;
      };
      server = mkOption {
        type = types.str;
      };
      port = mkOption {
        type = types.port;
        default = 1883;
      };
      user = mkOption {
        type = types.str;
      };
      passwordFile = mkOption {
        type = passwordFileType;
      };
    };
  };

  settings = {
    update_interval = 5;
    update_on_change = false;
    hass_url = null;
    hass_api = null;
    modules = enabledAddonsNames;
    custom_modules = null;
    exclude = [
    ];
    mqtt = {
      prefix = "lnxlink";
      clientId = cfg.mqtt.clientId;
      server = cfg.mqtt.server;
      port = cfg.mqtt.port;
      auth = {
        user = cfg.mqtt.user;
        pass = passwordPlaceholder;
        tls = false;
        keyfile = "";
        certfile = "";
        ca_certs = "";
      };
      discovery = {
        enabled = true;
      };
      lwt = {
        enabled = true;
        qos = 1;
        retain = false;
      };
    };
    settings = {
      systemd = null;
      gpio = {
        inputs = null;
        outputs = null;
      };
      hotkeys = null;
      disk_usage = {
        include_disks = [];
        exclude_disks = [];
      };
      statistics = "https://analyzer.bkbilly.workers.dev"; # XXX ?
      bash = {
        allow_any_command = false;
        expose = null;
      };
      mounts = {
        autocheck = false;
        directories = [];
      };
      ir_remote = {
        receiver = null;
        transmitter = null;
        buttons = [];
      };
      restful = {
        port = 8112;
      };
    };
  };

  configFileWithoutSecrets = yaml.generate "lnxlink-config-without-secrets.yaml" settings;

  renderSecrets = pkgs.writeShellScript "render-lnxlink-secrets" ''
      set -o errexit -o pipefail -o nounset
      target_file="$1"
      password_file="$2"

      ${lib.getExe' pkgs.coreutils "install"} -D ${configFileWithoutSecrets} $target_file
      ${lib.getExe pkgs.replace-secret} '${passwordPlaceholder}' "$password_file" "$target_file"
    '';

  addonOptions = nm:
    let
      meta = cfg.package.meta.addons.getMeta nm;
      variants = ({variants ? {}, ...}: builtins.attrNames variants) meta;
      maybeVariantOption =
        if (builtins.length variants > 0)
        then {
          variant = with lib; mkOption {
            type = types.enum variants;
            default = null;
            description = ''
              Which variant of this addon to enable (e.g. 'amd' or 'nvidia' for 'gpu' addon).
            '';
          };
        }
        else {};
    in {
      name = nm;
      value = {
        enable = lib.mkEnableOption "Enable addon ${nm}";
      } // maybeVariantOption;
    };

  enabledAddonsNames = builtins.filter (x: cfg.addons."${x}".enable) allAddons;

  # XXX variants
  finalPackage = cfg.package.override { addons = enabledAddonsNames; };

in
{
  options.services.lnxlink = {
    enable = lib.mkEnableOption "Enable LNXlink - linux integration for MQTT/Home Assistant";

    addons = builtins.listToAttrs (builtins.map addonOptions allAddons);

    logLevel = lib.mkOption {
      type = lib.types.enum [ "DEBUG" "INFO" "WARNING" "ERROR" "CRITICAL" ];
      default = "INFO";
    };

    mqtt = lib.mkOption {
      type = mqttModule;
      default = {};
      example = lib.literalExpression ''
        {
          user = "<username>";
          passwordFile = "/var/run/secrets/hass-mqtt-password";
          clientId = "<desktop-machine-name>";
          server = "192.168.0.1";
        }
      '';
    };

    configFile = with lib; mkOption {
      type = types.either types.str types.path;
      default = "${config.xdg.configHome}/lnxlink/config.yaml";
      defaultText = "$XDG_CONFIG_HOME/lnxlink/config.yaml";
      description = ''
        Path to the configuration file read by lnxlink (with passwords expanded).
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage "${self}/packages/lnxlink.nix" {};
    };

    daemonPath = with lib; mkOption {
      default = [
        pkgs.coreutils
      ];
      type = with types; listOf (oneOf [ package str ]);
      description = ''
        Packages added to the service's {env}`PATH`
        environment variable.  Both the {file}`bin`
        and {file}`sbin` subdirectories of each
        package are added.
      '';
    };
  };

  config = lib.mkIf cfg.enable ({
    # XXX assertions = [ <variant-is-chosen-for-enabled-modules-with-variants>  ];

    systemd.user.services.lnxlink = {
      Unit = {
        Description = "LNXlink";
        After = [ "network-online.target" "multi-user.target" "graphical.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Environment = [ ''PATH="${lib.makeBinPath cfg.daemonPath}:${lib.makeSearchPathOutput "bin" "sbin" cfg.daemonPath}"'' ];
        Type = "simple";
        LoadCredential = "mqtt-password:${cfg.mqtt.passwordFile}";
        Restart = "always";
        RestartSec = 5;
        ExecStartPre = ''
          ${renderSecrets} "%E/lnxlink/config.yaml" "%d/mqtt-password"
        '';
        ExecStart = ''
          ${lib.getExe finalPackage} --ignore-systemd --config "%E/lnxlink/config.yaml" --logging ${cfg.logLevel}
        '';
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  });
}
