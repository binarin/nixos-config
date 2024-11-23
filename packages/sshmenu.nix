{flake, ...}:
{ lib, writeTextFile, zsh, rxvt-unicode, xxd, ... }:
let
  runtimeInputs = [
    rxvt-unicode
    xxd
  ];
in
writeTextFile {
  name = "sshmenu";
  destination = "/bin/sshmenu";
  executable = true;
  text = ''
    #!${lib.getExe zsh}
    export PATH="${lib.makeBinPath runtimeInputs}:$PATH"

  '' + (builtins.readFile "${flake.inputs.self}/files/sshmenu");
}
