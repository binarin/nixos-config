{ writeShellScriptBin, writeTextFile, emacs, lib, ... }:
let
  isolatedCompiler = writeTextFile {
    name = "tangle-and-compile-emacs-config.el";
    text = builtins.readFile ./byte-compile.el;
  };
in
writeShellScriptBin "tangle-emacs-org-babel-config" ''
  ${lib.getExe emacs} --batch --load "${isolatedCompiler}" "$@"
''
