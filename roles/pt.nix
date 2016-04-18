{config, pkgs}:

let deps = rec {
  pt = pkgs.goPackages.ansicolor;
} in
deps.pt;
