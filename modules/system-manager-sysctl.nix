{ self, ... }:
{
  flake.systemModules.sysctl =
    { lib, pkgs, config, ... }:
    let
      cfg = config.boot.kernel.sysctl;

      sysctlOption = lib.mkOptionType {
        name = "sysctl option value";
        check =
          val:
          let
            checkType = x: lib.isBool x || lib.isString x || lib.isInt x || x == null;
          in
          checkType val || (val._type or "" == "override" && checkType val.content);
        merge = loc: defs: lib.mergeOneOption loc defs;
      };
    in
    {
      options.boot.kernel.sysctl = lib.mkOption {
        type = lib.types.attrsOf sysctlOption;
        default = { };
        example = lib.literalExpression ''
          { "net.ipv4.tcp_syncookies" = false; "vm.swappiness" = 60; }
        '';
        description = ''
          Runtime parameters of the Linux kernel, as set by
          {manpage}`sysctl(8)`.  Note that sysctl
          parameters names must be enclosed in quotes
          (e.g. `"vm.swappiness"` instead of
          `vm.swappiness`).  The value of each
          parameter may be a string, integer, boolean, or null
          (signifying the option will not appear at all).
        '';
      };

      config = lib.mkIf (cfg != { }) {
        environment.etc."sysctl.d/60-system-manager.conf".text = lib.concatStrings (
          lib.mapAttrsToList (
            n: v: lib.optionalString (v != null) "${n}=${if v == false then "0" else toString v}\n"
          ) cfg
        );

        systemd.services.apply-sysctl = {
          wantedBy = [ "system-manager.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = "${pkgs.procps}/bin/sysctl --system";
          };
        };
      };
    };
}
