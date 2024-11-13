{ flake, lib, config, options, osConfig ? null, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;

  featureDeps = {
    cad = [ "gui" ];
    gui = [ ];
    hyprland = [ "wayland" "gui" ];
    lnxlink = [ ];
    bleeding = [ ];
    fast-rebuild = [ ];
    wayland = [ "gui" ];
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

  featureEnabled = with builtins; with lib; feature:
    elem feature cfg.features ||
    any (dep: elem dep cfg.features) enableFeatureWhen."${feature}";

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

      feature = lib.genAttrs allFeatures (featureName:
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

      deployHostName = lib.mkOption {
        type = lib.types.nonEmptyStr;
      };

      validDeployTargets = lib.mkOption {
        type = lib.types.listOf lib.types.nonEmptyStr;
      };

      ipAllocation =
        if options.inventory.ipAllocation ? "${config.inventoryHostName}"
        then options.inventory.ipAllocation."${config.inventoryHostName}"
        else null;
    };
  };

  config = {
    hostConfig.feature = lib.genAttrs allFeatures (feat: lib.mkDefault (featureEnabled feat));
    hostConfig.hostId = host-ids."${config.inventoryHostName}";
    hostConfig.validDeployTargets = [
      config.hostConfig.deployHostName
    ];
  };
}
