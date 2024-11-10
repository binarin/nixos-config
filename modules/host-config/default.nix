{flake, lib, config, osConfig ? null, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  cfg = config.hostConfig;

  featureDeps = {
    gui = [];
    hyprland = ["wayland" "gui"];
    lnxlink = [];
    bleeding = [];
    fast-rebuild = [];
    wayland = ["gui"];
  };

  enableFeatureWhen =
    let
      # [ {name = "hyprland"; value = ["wayland" "gui"]}
      #   {name = "wayland"; value = ["gui"]} ]
      list = lib.attrsToList featureDeps;

      # [ { gui = "hyprland"; wayland = "hyrpland"; }
      #   { gui = "wayland"; } ]
      attrSets = builtins.map ({name, value}: lib.genAttrs value (_: name)) list;

      # { gui = ["hyrpland" "wayland"]; wayland = ["hyprland"]; }
      withDeps = lib.foldAttrs (v: acc: [v] ++ acc) [] attrSets;

      allEmpty = lib.genAttrs allFeatures (_: []);

    in
      allEmpty // withDeps;

  featureEnabled = with builtins; with lib; feature:
    elem feature cfg.features ||
    any (dep: elem dep cfg.features) enableFeatureWhen."${feature}";

  allFeatures = builtins.attrNames featureDeps;

  templates = rec {
    networkPrefix = {
      _type = "leaf";
      type = lib.types.ints.between 0 32;
    };

    nonEmptyStr = {
      _type = "leaf";
      type = lib.types.nonEmptyStr;
    };

    listOf = type: {
      _type = "leaf";
      default = [];
      type = lib.types.listOf (leafType type);
    };

    nullOr = type: {
      _type = "leaf";
      default = null;
      type = lib.types.nullOr (leafType type);
    };

    feature = {
      _type = "leaf";
      default = false;
      type = lib.types.bool;
    };

    leafType = type:
        if type ? _type && type._type == "leaf"
        then type.type
        else type;

    mkOpt = path: leaf:
      let
        description = "Option ${lib.concatStringsSep "." path}";
        type = leafType leaf;
        defaultAttrs =
          if leaf ? _type && leaf._type == "leaf" && leaf ? default
          then { default = leaf.default; }
          else {};
      in
      lib.mkOption ({
        inherit type description;
      } // defaultAttrs);

    stopAtTypes = val: lib.isAttrs val && !(val ? _type);

    mkOptions = template:
      lib.mapAttrsRecursiveCond stopAtTypes mkOpt template;

    mkSubmodule = template:
      lib.types.submodule {
        options = mkOptions template;
      };
  };

  networkSubmodule = with templates; mkSubmodule {
    prefix = networkPrefix;
    network = nonEmptyStr;
    dns = listOf nonEmptyStr;
    gateway = nullOr nonEmptyStr;
    domain = nullOr nonEmptyStr;
  };

  interfaceSubmodule = with templates; mkSubmodule {
    network = networkSubmodule;
    address = nonEmptyStr;
    mac = nullOr nonEmptyStr;
  };
in
{
  options = {
    inventoryHostName = lib.mkOption {
      type = lib.types.str;
    };

    hostConfig = {
      managedUsers = lib.mkOption {
        type = with lib.types; listOf nonEmptyStr;
        default = [];
      };

      features = lib.mkOption {
        type = lib.types.listOf (lib.types.enum allFeatures);
        default = [];
      };

      feature = lib.genAttrs allFeatures (featureName:
        lib.mkOption {
          type = lib.types.bool;
          description = ''
            Unpacking `hostConfig.features` into per-feature bool for easier access.
          '';
        }
      );

      ipam.interfaces = lib.mkOption {
        type = lib.types.attrsOf interfaceSubmodule;
      };
    };
  };
  config = {
    hostConfig.ipam.interfaces = self.lib.hostConfig.getIpam config.inventoryHostName;
    hostConfig.feature = lib.genAttrs allFeatures (feat: lib.mkDefault (featureEnabled feat));
  };
}
