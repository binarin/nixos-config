{ self, ... }:
{
  flake.systemModules.chrome-policies =
    { lib, pkgs, config, ... }:
    let
      cfg = config.programs.chromium;
      policyFile = pkgs.writeText "chrome-policies-extra.json" (builtins.toJSON cfg.extraOpts);
    in
    {
      options.programs.chromium.extraOpts = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };

      config = lib.mkIf (cfg.extraOpts != { }) {
        environment.etc."opt/chrome/policies/managed/extra.json".source =
          "${policyFile.overrideAttrs { preferLocalBuild = true; }}";
      };
    };
}
