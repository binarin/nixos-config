{ flake, pkgs, lib, ... }: {
  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      alwaysTangle = true;
      config = pkgs.runCommand "cleanup-unicode-from-emacs-config.org" { } ''
        ${lib.getExe' pkgs.coreutils "tr"} -c -d '\000-\199'< ${flake.inputs.self + "/users/emacs-config.org"} > $out
      '';
    }
    )
  ];
}
