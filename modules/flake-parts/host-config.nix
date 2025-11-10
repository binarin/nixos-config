{
  self,
  inputs,
  ...
}:
{
  nixosSharedModules = [ self.nixosModules.host-config ];

  flake.modules.generic.host-config =
    {
      flake,
      lib,
      config,
      options,
      ...
    }:
    let
      cfg = config.hostConfig;

      featureDeps = {
        hyprland = [
          "wayland"
          "gui"
        ];

        wayland = [ "gui" ];

        gui = [ ];

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
        tailscale = [ ];
        airgapped = [ ];
        secure-boot = [ ];
        emacs = [ "bleeding" ];
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
    in
    {
      options = {
        hostConfig = {
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
        };
      };

      config = lib.mkMerge [
        {
          hostConfig.feature = lib.genAttrs allFeatures (feat: lib.mkDefault (featureEnabled feat));
        }
      ];
    };

  flake.nixosModules.host-config =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.nixos.host-config";

      imports = [ self.modules.generic.host-config ];

      config = lib.mkMerge [
        {
          home-manager.sharedModules = [ self.modules.generic.host-config ];
        }
      ];
    };
}
