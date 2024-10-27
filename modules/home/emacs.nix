{ flake, pkgs, lib, ... }: {
  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      alwaysTangle = true;
      config = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
        ${lib.getExe' pkgs.coreutils "tr"} -c -s '\000-\177' x < ${flake.inputs.self + "/users/emacs-config.org"} > $out
      '';
    }
    )
  ];
}
