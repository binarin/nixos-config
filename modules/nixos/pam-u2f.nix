{flake, lib, pkgs, config, ...}:
let
  inherit (flake) inputs;
  inherit (inputs) self;

  cfg = config.security.pam-u2f;
  helpers = self.helpers.pam-u2f;

  pamServiceModule = with lib; with types; submodule {
    options = {
      enable = mkEnableOption "Enable pam_u2f for this pam service";
    };
  };
in
{
  options.security.pam-u2f.services = lib.mkOption {
    type = lib.types.attrsOf pamServiceModule;
    default = {};
  };

  # can't feed directly to mkMerge (infinite recursion), need to unroll a bit
  config = let
    enabledSvcs = lib.filterAttrs (n: o: o.enable) cfg.services;
  in {

    environment.etc = lib.mapAttrs' (service: opts: {
      name = "u2f_mappings.d/${service}";
      value = {
        source = pkgs.writeText "${service}-pam_u2f_mappings" (helpers.u2f_mappings config.inventoryHostName service);
      };
    }) enabledSvcs;

    security.pam.services = lib.genAttrs (lib.attrNames enabledSvcs) (service: {
      text = lib.mkBefore ''
        auth sufficient ${pkgs.pam_u2f}/lib/security/pam_u2f.so authfile=/etc/u2f_mappings.d/${service} cue pinverification=1 origin=pam://${config.inventoryHostName} appid=pam://${service}
      '';
    });
  };
}
