{ self, lib, ... }:
let
  selfLib = self.lib.self;
in
{
  flake.overlays.shitty-shit-launcher = final: _prev: {
    shitty-shit-run = final.writeShellApplication {
      name = "shitty-shit-run";
      runtimeInputs = [
        final.systemd # systemctl, systemd-run
        final.xdg-dbus-proxy
        final.coreutils # env, mkdir, rm, sleep
      ];
      text = selfLib.read "bin/shitty-shit-run";
    };

    wrapShittyShit =
      pkg:
      {
        talk ? [ ],
        own ? [ ],
        see ? [ ],
      }:
      let
        flags = lib.escapeShellArgs (
          (map (n: "--talk=${n}") talk)
          ++ (map (n: "--own=${n}") own)
          ++ (map (n: "--see=${n}") see)
        );
        runner = lib.getExe final.shitty-shit-run;
      in
      final.runCommandLocal "${pkg.name}-shitty"
        {
          meta = (pkg.meta or { });
          passthru = (pkg.passthru or { }) // { unwrapped = pkg; };
        }
        ''
          # Writable clone of the package as a symlink tree.
          mkdir -p "$out"
          cp -as --no-preserve=mode "${pkg}/." "$out/"

          # Replace every bin/* with a wrapper that routes through the launcher.
          if [ -d "$out/bin" ]; then
            for bin in "$out"/bin/*; do
              name="$(basename "$bin")"
              rm -f "$bin"
              {
                echo "#!${final.runtimeShell}"
                echo "exec ${runner} ${flags} -- ${pkg}/bin/$name \"\$@\""
              } > "$bin"
              chmod +x "$bin"
            done
          fi

          # Repoint every .desktop Exec= first token at our wrapper, preserving
          # trailing args and field codes (handles Desktop Actions too).
          if [ -d "$out/share/applications" ]; then
            for d in "$out"/share/applications/*.desktop; do
              [ -e "$d" ] || continue
              ${final.gawk}/bin/awk -v out="$out" '
                /^Exec=/ {
                  cmd = substr($0, 6)
                  n = split(cmd, a, " ")
                  exe = a[1]; sub(/.*\//, "", exe)
                  rest = ""
                  for (i = 2; i <= n; i++) rest = rest " " a[i]
                  print "Exec=" out "/bin/" exe rest
                  next
                }
                { print }
              ' "$d" > "$d.tmp"
              mv "$d.tmp" "$d"
            done
          fi
        '';
  };

  flake.homeModules.shitty-shit-slice =
    { config, lib, ... }:
    {
      key = "nixos-config.modules.home.shitty-shit-slice";

      options.programs.shitty-shit-slice.enable =
        lib.mkEnableOption "shitty-shit.slice (memory-capped Slack + Chrome)";

      config = lib.mkIf config.programs.shitty-shit-slice.enable {
        xdg.configFile."systemd/user/shitty-shit.slice".text = ''
          [Unit]
          Description=Memory-capped slice for Slack + Chrome

          [Slice]
          MemoryHigh=8G
          MemoryMax=10G
          MemorySwapMax=0
        '';
      };
    };
}
