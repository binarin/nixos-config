{lib, self}:

with lib;

let
  # pamu2fcfg -opam://<INVENTORY-HOSTNAME> -ipam://<PAM-SERVICE> --resident --pin-verification

  getAttrDefault = name: default: attrs: if hasAttr name attrs then getAttr name attrs else default;

  allMappings = import "${self}/inventory/priv-pam-u2f-keys.nix";

  relevantMappings = filterOrigin: filterAppId: lib.filter ({origin, appid, ...}: origin == filterOrigin && appid == filterAppId);

  appendMappingToUser = mapping: username: acc: acc // { "${username}" = (getAttrDefault username [] acc) ++ [ mapping ]; };

  processSingleMapping = {mapping, users, ...}: acc: foldr (appendMappingToUser mapping) acc users;

  u2f_mappings = origin: appid: pipe allMappings [
    attrValues
    (relevantMappings origin appid)
    (foldr processSingleMapping {})
    attrsToList
    (map ({name, value}: "${name}:${concatStringsSep ":" value}"))
    (concatStringsSep "\n")
  ];
in {
  inherit u2f_mappings;
}
