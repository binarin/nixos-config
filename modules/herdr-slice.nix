{ ... }:
{
  flake.homeModules.herdr-slice =
    { config, lib, ... }:
    let
      slice = "app-herdr.slice";
      # herdr-core starts its holder and server as transient user units
      # (hc-hold-<hash>.service, hc-serve-<hash>.service) and offers no way to
      # choose their slice. A drop-in on the unit-name prefix applies to the
      # transient units too.
      moveToSlice = ''
        [Service]
        Slice=${slice}
      '';
    in
    {
      key = "nixos-config.modules.home.herdr-slice";

      options.programs.herdr-slice.enable =
        lib.mkEnableOption "app-herdr.slice (memory-capped herdr-core terminals)";

      config = lib.mkIf config.programs.herdr-slice.enable {
        # Named app-herdr.slice so systemd nests it under the user app.slice.
        xdg.configFile."systemd/user/${slice}".text = ''
          [Unit]
          Description=Memory-capped slice for herdr-core terminals

          [Slice]
          MemoryHigh=50%
          MemoryMax=60%
        '';
        xdg.configFile."systemd/user/hc-hold-.service.d/slice.conf".text = moveToSlice;
        xdg.configFile."systemd/user/hc-serve-.service.d/slice.conf".text = moveToSlice;
      };
    };
}
