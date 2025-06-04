{flake, lib, pkgs, config, ...}:
let
  yamlFormat = pkgs.formats.yaml { };
  cfg = config.programs.sbctl;
in {
  imports = [
    flake.inputs.lanzaboote.nixosModules.lanzaboote
  ];
  options = {
    programs.sbctl = {
      pkiBundle = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/sbctl";
      };
      config = lib.mkOption {
        type = yamlFormat.type;
        default = { };
      };
    };
  };
  config = lib.mkIf config.hostConfig.feature.secure-boot {
    environment.systemPackages = with pkgs; [
      sbctl
    ];
    environment.etc."sbctl/sbctl.conf".source = yamlFormat.generate "sbctl.conf" config.programs.sbctl.config;

    boot.loader.systemd-boot.enable = lib.mkForce false;

    boot.lanzaboote = {
      enable = true;
      pkiBundle = cfg.pkiBundle;
    };

    programs.sbctl.config = let
      fn = f: lib.mkDefault "${cfg.pkiBundle}/${f}";
    in {
      keydir = fn "keys";
      guid = fn "GUID";
      files_db = fn "files.json";
      bundles_db = fn "bundles.json";

      keys = {
        pk = {
          privkey = fn "keys/PK/PK.key";
          pubkey = fn "keys/PK/PK.pem";
          type = "file";
        };
        kek = {
          privkey = fn "keys/KEK/KEK.key";
          pubkey = fn "keys/KEK/KEK.pem";
          type = "file";
        };
        db = {
          privkey = fn "keys/db/db.key";
          pubkey = fn "keys/db/db.pem";
          type = "file";
        };
      };
    };
  };
}
