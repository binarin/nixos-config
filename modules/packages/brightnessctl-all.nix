{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.brightnessctl-all = pkgs.writeShellApplication {
        name = "brightnessctl-all";
        runtimeInputs = with pkgs; [
          ddcutil
          coreutils
        ];
        text = ''
          mapfile -t all < <(ddcutil detect --brief | perl -nE 'say $1 if m,/dev/i2c-(\d+),')
          pids=()
          for dev in "''${all[@]}" ; do
            ddcutil -b "$dev" setvcp 10 "$1" &
            pids[dev]=$!
          done
          for pid in "''${pids[@]}"; do
            wait "$pid"
          done
        '';
      };
    };
}
