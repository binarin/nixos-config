{ pkgs, lib, ... }:
pkgs.writeShellScriptBin "cleanup-unicode-from-emacs-org-babel-config" ''
    ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < "$@"
''
