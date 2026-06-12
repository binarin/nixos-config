{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.sops =
    { lib, pkgs, config, ... }:
    let
      cfg = config.sops;
      users = config.users.users;
      sops-install-secrets = cfg.package;
      manifestFor = pkgs.callPackage "${inputs.sops-nix}/modules/sops/manifest-for.nix" {
        inherit cfg;
        inherit (pkgs) writeTextFile;
      };
      manifest = manifestFor "" cfg.secrets cfg.templates { };

      pathNotInStore = lib.mkOptionType {
        name = "pathNotInStore";
        description = "path not in the Nix store";
        descriptionClass = "noun";
        check = x: !lib.path.hasStorePathPrefix (/. + x);
        merge = lib.mergeEqualOption;
      };

      withEnvironment = import "${inputs.sops-nix}/modules/sops/with-environment.nix" {
        cfg = lib.recursiveUpdate cfg {
          environment.HOME = "/var/empty";
          environment.PATH = lib.makeBinPath cfg.age.plugins;
        };
        inherit lib;
      };

      secretType = lib.types.submodule (
        { config, ... }:
        {
          config = {
            sopsFile = lib.mkOptionDefault cfg.defaultSopsFile;
            sopsFileHash = lib.mkOptionDefault (
              lib.optionalString cfg.validateSopsFiles "${builtins.hashFile "sha256" config.sopsFile}"
            );
          };
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              default = config._module.args.name;
              description = "Name of the file used in /run/secrets.";
            };
            key = lib.mkOption {
              type = lib.types.str;
              default = if cfg.defaultSopsKey != null then cfg.defaultSopsKey else config._module.args.name;
              description = "Key used to lookup in the sops file.";
            };
            path = lib.mkOption {
              type = lib.types.str;
              default = "/run/secrets/${config.name}";
              description = "Path where secrets are symlinked to.";
            };
            format = lib.mkOption {
              type = lib.types.enum [ "yaml" "json" "binary" "dotenv" "ini" ];
              default = cfg.defaultSopsFormat;
              description = "File format used to decrypt the sops secret.";
            };
            mode = lib.mkOption {
              type = lib.types.str;
              default = "0400";
              description = "Permissions mode in octal.";
            };
            owner = lib.mkOption {
              type = with lib.types; nullOr str;
              default = null;
              description = "User of the file.";
            };
            uid = lib.mkOption {
              type = with lib.types; nullOr int;
              default = 0;
              description = "UID of the file.";
            };
            group = lib.mkOption {
              type = with lib.types; nullOr str;
              default = if config.owner != null then users.${config.owner}.group or null else null;
              description = "Group of the file.";
            };
            gid = lib.mkOption {
              type = with lib.types; nullOr int;
              default = 0;
              description = "GID of the file.";
            };
            sopsFile = lib.mkOption {
              type = lib.types.path;
              description = "Sops file the secret is loaded from.";
            };
            sopsFileHash = lib.mkOption {
              type = lib.types.str;
              readOnly = true;
              description = "Hash of the sops file.";
            };
            restartUnits = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Units to restart when this secret changes.";
            };
            reloadUnits = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Units to reload when this secret changes.";
            };
            neededForUsers = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Decrypt before users are created.";
            };
          };
        }
      );

      templateType = lib.types.submodule (
        { config, ... }:
        {
          options = {
            name = lib.mkOption {
              type = lib.types.singleLineStr;
              default = config._module.args.name;
              description = "Name of the rendered file.";
            };
            path = lib.mkOption {
              type = lib.types.singleLineStr;
              default = "/run/secrets/rendered/${config.name}";
              description = "Path where the rendered file will be placed.";
            };
            content = lib.mkOption {
              type = lib.types.lines;
              default = "";
              description = "Content of the template file.";
            };
            mode = lib.mkOption {
              type = lib.types.singleLineStr;
              default = "0400";
              description = "Permissions mode in octal.";
            };
            owner = lib.mkOption {
              type = with lib.types; nullOr singleLineStr;
              default = null;
              description = "User of the file.";
            };
            uid = lib.mkOption {
              type = with lib.types; nullOr int;
              default = 0;
              description = "UID of the template.";
            };
            group = lib.mkOption {
              type = with lib.types; nullOr singleLineStr;
              default = if config.owner != null then users.${config.owner}.group or null else null;
              description = "Group of the file.";
            };
            gid = lib.mkOption {
              type = with lib.types; nullOr int;
              default = 0;
              description = "GID of the template.";
            };
            file = lib.mkOption {
              type = lib.types.path;
              default = pkgs.writeText config.name config.content;
              description = "File used as the template.";
            };
            restartUnits = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Units to restart when the template changes.";
            };
            reloadUnits = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Units to reload when the template changes.";
            };
          };
        }
      );
    in
    {
      options.sops = {
        secrets = lib.mkOption {
          type = lib.types.attrsOf secretType;
          default = { };
          description = "Secrets to decrypt and place at runtime.";
        };

        templates = lib.mkOption {
          type = lib.types.attrsOf templateType;
          default = { };
          description = "Templates for secret files.";
        };

        placeholder = lib.mkOption {
          type = lib.types.attrsOf (lib.types.mkOptionType {
            name = "coercibleToString";
            description = "value that can be coerced to string";
            check = lib.strings.isConvertibleWithToString;
            merge = lib.mergeEqualOption;
          });
          default = { };
          visible = false;
        };

        defaultSopsFile = lib.mkOption {
          type = lib.types.path;
          description = "Default sops file used for all secrets.";
        };

        defaultSopsFormat = lib.mkOption {
          type = lib.types.str;
          default = "yaml";
          description = "Default sops format used for all secrets.";
        };

        defaultSopsKey = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Default key used to lookup in all secrets.";
        };

        validateSopsFiles = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Check all sops files at evaluation time.";
        };

        keepGenerations = lib.mkOption {
          type = lib.types.ints.unsigned;
          default = 1;
          description = "Number of secrets generations to keep.";
        };

        log = lib.mkOption {
          type = lib.types.listOf (lib.types.enum [ "keyImport" "secretChanges" ]);
          default = [ "keyImport" "secretChanges" ];
          description = "What to log.";
        };

        environment = lib.mkOption {
          type = lib.types.attrsOf (lib.types.either lib.types.str lib.types.path);
          default = { };
          description = "Environment variables to set before calling sops-install-secrets.";
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = (pkgs.callPackage "${inputs.sops-nix}" { }).sops-install-secrets;
          description = "sops-install-secrets package to use.";
        };

        validationPackage = lib.mkOption {
          type = lib.types.package;
          default = sops-install-secrets;
          description = "sops-install-secrets package for validation.";
        };

        useTmpfs = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Use tmpfs instead of ramfs for secrets storage.";
        };

        age = {
          keyFile = lib.mkOption {
            type = lib.types.nullOr pathNotInStore;
            default = null;
            example = "/var/lib/sops-nix/key.txt";
            description = "Path to age key file.";
          };

          plugins = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            default = [ ];
            description = "Plugins for sops decryption.";
          };

          generateKey = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether to generate the age key.";
          };

          sshKeyPaths = lib.mkOption {
            type = lib.types.listOf lib.types.path;
            default =
              if config.services.openssh.enable or false then
                map (e: e.path) (
                  lib.filter (
                    e: e.type == "ed25519" && !(lib.hasPrefix "/run/secrets" e.path)
                  ) (config.services.openssh.hostKeys or [])
                )
              else
                [];
            description = "Paths to ssh keys added as age keys.";
          };
        };

        gnupg = {
          home = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Path to gnupg database directory.";
          };

          sshKeyPaths = lib.mkOption {
            type = lib.types.listOf lib.types.path;
            default = [];
            description = "Path to ssh keys added as GPG keys.";
          };

          package = lib.mkOption {
            type = lib.types.package;
            default = pkgs.gnupg;
            description = "The gnupg package.";
          };
        };
      };

      config = lib.mkIf (cfg.secrets != { } || cfg.templates != { }) {
        assertions = [
          {
            assertion =
              cfg.gnupg.home != null
              || cfg.gnupg.sshKeyPaths != [ ]
              || cfg.age.keyFile != null
              || cfg.age.sshKeyPaths != [ ];
            message = "No key source configured for sops. Set sops.age.keyFile or sops.age.sshKeyPaths.";
          }
        ];

        sops.environment.SOPS_GPG_EXEC = lib.mkIf (cfg.gnupg.home != null || cfg.gnupg.sshKeyPaths != [ ]) (
          lib.mkDefault "${cfg.gnupg.package}/bin/gpg"
        );

        sops.placeholder = lib.mkIf (cfg.templates != { }) (
          lib.mapAttrs (
            name: _: lib.mkDefault "<SOPS:${builtins.hashString "sha256" name}:PLACEHOLDER>"
          ) cfg.secrets
        );

        systemd.services.sops-install-secrets = {
          wantedBy = [ "system-manager.target" ];
          after = [ "userborn.service" ];
          before = [ "system-manager.target" ];
          environment = cfg.environment // {
            SOPS_RESTART_UNITS_VIA_SYSTEMCTL = "1";
          };
          path = cfg.age.plugins;
          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${sops-install-secrets}/bin/sops-install-secrets ${manifest}";
            RemainAfterExit = true;
          };
        };

        systemd.services.sops-generate-age-key = lib.mkIf cfg.age.generateKey {
          wantedBy = [ "system-manager.target" ];
          before = [ "sops-install-secrets.service" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = let
            escapedKeyFile = lib.escapeShellArg cfg.age.keyFile;
          in ''
            if [[ ! -f ${escapedKeyFile} ]]; then
              echo generating machine-specific age key...
              mkdir -p $(dirname ${escapedKeyFile})
              ${pkgs.age}/bin/age-keygen -o ${escapedKeyFile}
            fi
          '';
        };
      };
    };
}
