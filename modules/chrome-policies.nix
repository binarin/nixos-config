{ self, ... }:
{
  flake.modules.generic.chrome-policies =
    { lib, ... }:
    {
      key = "nixos-config.modules.generic.chrome-policies";
      config.programs.chromium.extraOpts = {
        ExternalProtocolDialogShowAlwaysOpenCheckbox = true;
        URLAllowlist = [
          "org-protocol://*"
          "bmeeting://*"
        ];
      };
    };
}
