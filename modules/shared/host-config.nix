{
  flake,
  lib,
  config,
  options,
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
    ];

    wayland = [
      "gui"
    ];

    gui = [
    ];

    workstation = [
      "gui"
      "interactive-cli"
      "emacs"
    ];

    vfio = [ ];
    lxc = [ ];
    lnxlink = [ ];
    bleeding = [ ];
    fast-rebuild = [ ];
    server = [ ];
    nix-builder = [ ];
    interactive-cli = [
      "bleeding"
      "emacs"
    ];
    wsl = [ ];
    impermanence = [ ];
    tailscale = [ ];
    airgapped = [ ];
    secure-boot = [ ];
    emacs = [
      "bleeding"
    ];
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
      lib = lib.mkOption {
        type = lib.types.attrsOf lib.types.attrs;
        default = { };
        description = ''
          Separate from top-level `config.lib` due to infinite
          recursion problems - I want introduce a custom mkOverride
          (with the meaning `enabled by hostConfig feature`). But some
          home-manager modules add things to `config.lib` with `mkIf`,
          and apparently putting my custom override also in
          `config.lib` is not an option - looks like `config.lib` is
          being evaluated too eagerly.
        '';
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

      ipAllocation = lib.mkOption {
        type = lib.types.raw;
        default = config.inventory.ipAllocation."${config.inventoryHostName}";
      };
    };
  };

  config = {
    hostConfig.feature = lib.genAttrs allFeatures (feat: lib.mkDefault (featureEnabled feat));
    hostConfig.hostId = host-ids."${config.inventoryHostName}";
    hostConfig.validDeployTargets = [ config.hostConfig.deployHostName ];
    hostConfig.lib.defaults.enable = lib.mkOverride 950 true;
  };
}
