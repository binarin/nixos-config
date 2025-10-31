{...}: {
  flake.nixosModules.niri = {pkgs, lib, config, ...}: let
    # wrapper script for `binPath` since the option type is `path`
    niriSession = lib.getExe (pkgs.writeShellScriptBin "niriSession" ''
        ${lib.getExe config.programs.niri.package } --session
      '');
  in {
    # Things used by the default config
    environment.systemPackages = with pkgs; [
      alacritty
      fuzzel
    ];

    programs.uwsm = {
      enable = true;
      waylandCompositors = {
        niri = {
          prettyName = "niri";
          comment = "niri compositor managed by UWSM";
          binPath = niriSession;
        };
      };
    };
  };
}
