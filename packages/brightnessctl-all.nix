{
  writeShellApplication,
  brightnessctl,
  coreutils,
  ...
}:
writeShellApplication {
  name = "brightnessctl-all";
  runtimeInputs = [
    brightnessctl
    coreutils
  ];
  text = ''
    mapfile -t all < <(brightnessctl -l -c backlight -m | cut -d , -f1)
    exit_code=0
    for dev in "''${all[@]}" ; do
      if ! brightnessctl -d "$dev" "$@"; then
        exit_code=$?
      fi
    done
    exit $exit_code
  '';
}
