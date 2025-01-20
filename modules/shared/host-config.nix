{
  flake,
  lib,
  config,
  options,
  osConfig ? null,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;

  featureDeps = {
    hyprland = [
      "wayland"
      "gui"
      "interactive-cli"
    ];
    wayland = [
      "gui"
      "interactive-cli"
    ];
    gui = [
      "interactive-cli"
    ];

    vfio = [ ];
    lxc = [ ];
    lnxlink = [ ];
    bleeding = [ ];
    fast-rebuild = [ ];
    server = [ ];
    nix-builder = [ ];
    interactive-cli = [ ];
    wsl = [ ];
    impermanence = [ ];
    tailscale = [ ];
  };

  defaultEnabled = {
    bleeding = true;
  };

  enableFeatureWhen =
    let
      # [ {name = "hyprland"; value = ["wayland" "gui"]}
      #   {name = "wayland"; value = ["gui"]} ]
      list = lib.attrsToList featureDeps;

      # [ { gui = "hyprland"; wayland = "hyrpland"; }
      #   { gui = "wayland"; } ]
      attrSets = builtins.map ({ name, value }: lib.genAttrs value (_: name)) list;

      # { gui = ["hyrpland" "wayland"]; wayland = ["hyprland"]; }
      withDeps = lib.foldAttrs (v: acc: [ v ] ++ acc) [ ] attrSets;

      allEmpty = lib.genAttrs allFeatures (_: [ ]);
    in
    allEmpty // withDeps;

  featureEnabled =
    with builtins;
    with lib;
    feature:
    elem feature cfg.features
    || any (dep: elem dep cfg.features) enableFeatureWhen."${feature}"
    || (hasAttr feature defaultEnabled && getAttr feature defaultEnabled);

  allFeatures = builtins.attrNames featureDeps;

  host-ids = import "${self}/inventory/host-id.nix";
in
{
  options = {
    hostConfig = {
      managedUsers = lib.mkOption {
        type = with lib.types; listOf nonEmptyStr;
        default = [ ];
      };

      features = lib.mkOption {
        type = lib.types.listOf (lib.types.enum allFeatures);
        default = [ ];
      };

      feature = lib.genAttrs allFeatures (
        featureName:
        lib.mkOption {
          type = lib.types.bool;
          description = ''
            Unpacking "${featureName} from "`hostConfig.features` into per-feature bool for easier access.
          '';
        }
      );

      hostId = lib.mkOption {
        type = lib.types.nonEmptyStr;
        readOnly = true;
      };

      deployHostName = lib.mkOption { type = lib.types.nullOr lib.types.nonEmptyStr; };

      validDeployTargets = lib.mkOption { type = lib.types.listOf lib.types.nonEmptyStr; };

      ipAllocation =
        if options.inventory.ipAllocation ? "${config.inventoryHostName}" then
          options.inventory.ipAllocation."${config.inventoryHostName}"
        else
          lib.mkOption {
            default = { };
            type = lib.types.attrs;
          };
    };
  };

  config = {
    hostConfig.feature = lib.genAttrs allFeatures (feat: lib.mkDefault (featureEnabled feat));
    hostConfig.hostId = host-ids."${config.inventoryHostName}";
    hostConfig.validDeployTargets = [ config.hostConfig.deployHostName ];
  };
}
