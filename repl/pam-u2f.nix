let
  self = "/home/binarin/personal-workspace/nixos-config";
  pam-u2f = import "${self}/modules/helpers/pam-u2f.nix" {
    lib = import <nixpkgs/lib>;
    inherit self;
  };
  t = pam-u2f.u2f_mappings "demandred" "sddm";
in {
  inherit t;
}
