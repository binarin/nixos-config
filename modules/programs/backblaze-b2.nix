{ self, ... }:
{
  flake.homeModules.backblaze-b2 =
    {
      lib,
      pkgs,
      osConfig,
      config,
      ...
    }:
    let
      persistDir = "/persist${config.home.homeDirectory}/.config/b2";
    in
    {
      key = "nixos-config.modules.home.backblaze-b2";
      config = {
        home.packages = with pkgs; [ backblaze-b2 ];
        home.sessionVariables = lib.mkIf osConfig.impermanence.enable {
          B2_ACCOUNT_INFO = persistDir;
        };
        home.activation = lib.mkIf osConfig.impermanence.enable {
          b2-account-persist-dir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            mkdir -p "${persistDir}" || true
          '';
        };
      };
    };
}
