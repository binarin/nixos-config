{ lib, self, ... }:
let
  readRawInventory =
    let
      dir = builtins.readDir "${self}/inventory/networks";
      regularTomlFiles = lib.attrNames (
        lib.filterAttrs (n: v: v == "regular" && lib.hasSuffix ".toml" n) dir
      );
      val = lib.map (
        fn:
        let
          netName = lib.removeSuffix ".toml" fn;
        in
        lib.nameValuePair netName (builtins.fromTOML (builtins.readFile "${self}/inventory/networks/${fn}"))
      ) regularTomlFiles;
    in
    lib.listToAttrs val;

  buildHostLookupTable =
    rawInventory:
    let
      perNetwork = lib.attrValues (lib.mapAttrs mkReverseLookupTable rawInventory);
    in
    lib.zipAttrsWith (_hostName: networkTags: lib.foldr (a: b: a // b) { } networkTags) perNetwork;

  mkReverseLookupTable =
    netName:
    { ipam, ... }:
    let
      hostLookup = lib.zipAttrsWith (_: mergeTagAssignments) (allAssignmentsForNetwork ipam);
    in
    lib.mapAttrs (_hostName: tags: { "${netName}" = tags; }) hostLookup;

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
      # Simple format: just hostname, implies "primary" tag
      {
        hostname = hostNameWithTags;
        tags = [ "primary" ];
        mac = null;
      }
    else if lib.isAttrs hostNameWithTags then
      # New hash format: { hostname = "..."; tags = [...]; mac = "..."; }
      {
        hostname = hostNameWithTags.hostname;
        tags = hostNameWithTags.tags or [ "primary" ];
        mac = hostNameWithTags.mac or null;
      }
    else
      # Legacy array format: [hostname, tag1, tag2, ...]
      {
        hostname = lib.head hostNameWithTags;
        tags = lib.tail hostNameWithTags;
        mac = null;
      };

  normalizeIpam = lib.mapAttrs (_k: v: expandIpAllocationTarget v);

  taggedHostnames =
    { domain, ... }:
    allocationRecord:
    with lib;
    let
      hostname = allocationRecord.hostname;
      tags = allocationRecord.tags;
    in
    map (
      tag: if tag == "primary" then "${hostname}.${domain}" else "${hostname}-${tag}.${domain}"
    ) tags;

  assignIp =
    allocationRecord: ip:
    let
      hostName = allocationRecord.hostname;
      tags = allocationRecord.tags;
      mac = allocationRecord.mac;
    in
    {
      "${hostName}" = lib.genAttrs tags (_tag: {
        _type = "leaf";
        address = ip;
        inherit mac;
      });
    };
in
{
  inherit
    readRawInventory
    buildHostLookupTable
    normalizeIpam
    taggedHostnames
    ;
}
