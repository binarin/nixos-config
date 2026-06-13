{
  lib,
  nixos,
  userborn,
  system-manager-src,
}:
let
  self = {
    makeSystemConfig =
      {
        pkgs,
        modules,
        specialArgs ? { },
      }:
      let
        extraArgsModule =
          {
            lib,
            config,
            ...
          }:
          {
            _module.args = {
              inherit pkgs;
              utils =
                let
                  nixosUtils = import "${nixos}/lib/utils.nix" {
                    inherit lib config pkgs;
                  };
                in
                nixosUtils
                // {
                  toShellPath =
                    shell:
                    if lib.types.shellPackage.check shell then
                      "/run/system-manager/sw${shell.shellPath}"
                    else if lib.types.package.check shell then
                      throw "${shell} is not a shell package"
                    else
                      shell;
                };
              system-manager = pkgs.callPackage "${system-manager-src}/nix/packages/wrapper.nix" {
                system-manager-unwrapped = pkgs.callPackage "${system-manager-src}/package.nix" { };
              };
              userborn = userborn.packages.${pkgs.system}.default;
            };
          };

        config =
          (lib.evalModules {
            specialArgs = {
              nixosModulesPath = "${nixos}/modules";
            } // specialArgs;
            modules = [
              extraArgsModule
              "${system-manager-src}/nix/modules"
              {
                nixpkgs.hostPlatform = pkgs.system;
                build = { inherit toplevel; };
              }
            ]
            ++ modules;
          }).config;

        system-manager = pkgs.callPackage "${system-manager-src}/nix/packages/wrapper.nix" {
          system-manager-unwrapped = pkgs.callPackage "${system-manager-src}/package.nix" { };
        };

        returnIfNoAssertions =
          drv:
          let
            failedAssertions = map (x: x.message) (lib.filter (x: !x.assertion) config.assertions);
          in
          if failedAssertions != [ ] then
            throw "\nFailed assertions:\n${lib.concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
          else
            lib.showWarnings config.warnings drv;

        servicesPath = pkgs.writeTextFile {
          name = "services";
          destination = "/services.json";
          text = lib.generators.toJSON { } config.build.services;
        };

        etcPath = pkgs.writeTextFile {
          name = "etcFiles";
          destination = "/etcFiles.json";
          text = lib.generators.toJSON { } { inherit (config.build.etc) entries staticEnv; };
        };

        linkFarmNestedEntryFromDrv = dirs: drv: {
          name = lib.concatStringsSep "/" (dirs ++ [ "${drv.name}" ]);
          path = drv;
        };
        linkFarmEntryFromDrv = linkFarmNestedEntryFromDrv [ ];
        linkFarmBinEntryFromDrv = linkFarmNestedEntryFromDrv [ "bin" ];

        toplevel =
          let
            scripts = lib.mapAttrsToList (_: script: linkFarmBinEntryFromDrv script) config.build.scripts;

            engineEntry = {
              name = "bin/system-manager-engine";
              path = "${system-manager}/bin/system-manager-engine";
            };

            checks = lib.imap0 (i: check: {
              name = "checks/${toString i}-${check.name}";
              path = check;
            }) config.system.checks;

            entries = [
              (linkFarmEntryFromDrv servicesPath)
              (linkFarmEntryFromDrv etcPath)
              engineEntry
            ]
            ++ scripts
            ++ checks;

            addPassthru =
              drv:
              drv.overrideAttrs (prevAttrs: {
                passthru = (prevAttrs.passthru or { }) // {
                  inherit config;
                };
              });
          in
          addPassthru (pkgs.linkFarm "system-manager" entries);
      in
      returnIfNoAssertions toplevel;
  };
in
self
