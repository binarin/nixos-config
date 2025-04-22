{ lib, self, ... }:
let
  readRawInventory =
    let
      dir = builtins.readDir "${self}/inventory/networks";
      regularNixFiles = lib.attrNames (
        lib.filterAttrs (n: v: v == "regular" && lib.hasSuffix ".nix" n) dir
      );
      val = lib.map (
        fn:
        let
          netName = lib.removeSuffix ".nix" fn;
        in
        lib.nameValuePair netName (import "${self}/inventory/networks/${fn}")
      ) regularNixFiles;
    in
      lib.listToAttrs val;

  buildHostLookupTable = rawInventory:
    let
      perNetwork = lib.attrValues (lib.mapAttrs mkReverseLookupTable rawInventory);
    in
    lib.zipAttrsWith (hostName: networkTags: lib.foldr (a: b: a // b) { } networkTags) perNetwork;

  mkReverseLookupTable =
    netName:
    { ipam, ... }:
    let
      hostLookup = lib.zipAttrsWith (_: mergeTagAssignments) (allAssignmentsForNetwork ipam);
    in
    lib.mapAttrs (hostName: tags: { "${netName}" = tags; }) hostLookup;

  mergeTagAssignments = lib.zipAttrsWith (
    name: values:
    if builtins.length values == 1 then
      builtins.head values
    else
      throw "Tag '${name}' conflict ${builtins.toJSON values}"
  );

  allAssignmentsForNetwork =
    ipam:
    lib.map (
      { name, value }:
      let
        ip = name;
        allocationTarget = expandIpAllocationTarget value;
      in
      assignIp allocationTarget ip
    ) (lib.attrsToList ipam);

  expandIpAllocationTarget =
    hostNameWithTags:
    if lib.isString hostNameWithTags then
      [
        hostNameWithTags
        "primary"
      ]
    else
      hostNameWithTags;

  normalizeIpam = lib.mapAttrs (k: v: expandIpAllocationTarget v);

  taggedHostnames = {domain, ...}: hostnameWithTags:
    with lib;
    let
      hostname = head hostnameWithTags;
      tags = tail hostnameWithTags;
    in
      map (tag: if tag == "primary" then "${hostname}.${domain}" else "${hostname}-${tag}.${domain}") tags;

  assignIp =
    ipAllocationTarget: ip:
    let
      hostName = lib.head ipAllocationTarget;
      tags = lib.tail ipAllocationTarget;
    in
    {
      "${hostName}" = lib.genAttrs tags (tag: {
        _type = "leaf";
        address = ip;
      });
    };

  onlyKey =
    attrs:
    let
      keys = lib.attrNames attrs;
      only = lib.head keys;
    in
    if lib.length keys == 1 then
      only
    else
      throw "Should be a singleton attrSet, but has ${builtins.toJSON keys}";
in {
  inherit readRawInventory buildHostLookupTable;
}
