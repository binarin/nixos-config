{writeShellScriptBin, emacs, lib, ...}:

writeShellScriptBin "tangle-emacs-org-babel-config" ''
  ${lib.getExe emacs} --batch --load ${./byte-compile.el} "$@"
''
