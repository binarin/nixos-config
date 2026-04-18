{ self, ... }:
let
  selfLib = self.lib.self;
  niriCargo = builtins.fromTOML (selfLib.read' "packages/niri-dynamic-keybindings/Cargo.toml");
  packageFn =
    { rustPlatform, lib }:
    rustPlatform.buildRustPackage {
      pname = "niri-dynamic-keybindings";
      version = "0.1.0";
      src = selfLib.dir' "packages/niri-dynamic-keybindings";
      cargoLock = {
        lockFile = selfLib.file' "packages/niri-dynamic-keybindings/Cargo.lock";
        outputHashes."niri-ipc-25.11.0" = niriCargo.dependencies.niri-ipc.ipcHash;
      };
      meta.mainProgram = "niri-dynamic-keybindings";
    };
in
{
  perSystem =
    { pkgs, lib, ... }:
    {
      packages.niri-dynamic-keybindings = pkgs.callPackage packageFn { };
    };

  flake.overlays.niri-dynamic-keybindings = final: prev: {
    niri-dynamic-keybindings = final.callPackage packageFn { };
  };

  flake.homeModules.niri-dynamic-keybindings =
    {
      self',
      config,
      lib,
      ...
    }:
    let
      cfg = config.services.niri-dynamic-keybindings;
    in
    {
      key = "nixos-config.modules.home.niri-dynamic-keybindings";
      options.services.niri-dynamic-keybindings = with lib; {
        enable = mkOption {
          type = types.bool;
          default = true;
        };
        package = mkOption {
          type = types.package;
          default = self'.packages.niri-dynamic-keybindings;
        };
        systemdTarget = mkOption {
          type = types.str;
          default = config.wayland.systemd.target;
        };
        targetFile = mkOption {
          type = types.str;
          default = "${config.xdg.configHome}/niri/dynamic-binds.kdl";
        };
        logLevel = mkOption {
          type = types.enum [
            "warn"
            "error"
            "info"
            "debug"
            "trace"
          ];
          default = "info";
        };
      };
      config = {
        systemd.user.services.niri-dynamic-keybindings = {
          Install.WantedBy = [ cfg.systemdTarget ];
          Unit = {
            ConditionEnvironment = "NIRI_SOCKET";
            After = [ cfg.systemdTarget ];
            BindsTo = [ cfg.systemdTarget ];
          };
          Service = {
            Type = "simple";
            Restart = "always";
            Environment = [ "RUST_LOG=${cfg.logLevel}" ];
            ExecStart = ''
              ${lib.getExe cfg.package} --target-file ${lib.escapeShellArg cfg.targetFile}
            '';
          };
        };
      };
    };
}
