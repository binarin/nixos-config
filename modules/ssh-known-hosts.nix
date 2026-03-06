{
  self,
  config,
  lib,
  ...
}:
let
  flakeConfig = config;

  # Read secrets directory to find machines with SSH host keys
  secretsDir = "${self}/secrets";
  secretsDirContents = builtins.readDir secretsDir;

  # Get list of machine directories
  machineDirs = lib.filterAttrs (_name: type: type == "directory") secretsDirContents;

  # For each machine, find SSH host public keys
  machineKeys = lib.mapAttrs (
    machineName: _:
    let
      machineDir = "${secretsDir}/${machineName}";
      dirContents = builtins.readDir machineDir;
      sshKeyFiles = lib.filterAttrs (
        name: type: type == "regular" && lib.hasPrefix "ssh_host_" name && lib.hasSuffix "_key.pub" name
      ) dirContents;
    in
    lib.mapAttrs (
      keyFile: _:
      let
        keyContent = lib.trim (builtins.readFile "${machineDir}/${keyFile}");
        # Extract key type from filename: ssh_host_ed25519_key.pub -> ed25519
        keyType = lib.removePrefix "ssh_host_" (lib.removeSuffix "_key.pub" keyFile);
      in
      {
        inherit keyType keyContent;
        file = keyFile;
      }
    ) sshKeyFiles
  ) machineDirs;

  # Filter to only machines that have SSH keys
  machinesWithKeys = lib.filterAttrs (_name: keys: keys != { }) machineKeys;

  # Get all IP addresses for a machine from inventory (across all networks)
  getHostIps =
    hostname:
    let
      hostAllocation = flakeConfig.inventory.ipAllocation.${hostname} or { };
      # hostAllocation is: { home = { primary = { address = "..."; }; }; guest = { ... }; }
      # We need to collect all addresses from all networks and all tags
      collectAddresses =
        alloc:
        lib.flatten (
          lib.mapAttrsToList (
            _networkName: tags: lib.mapAttrsToList (_tagName: tagData: tagData.address or null) tags
          ) alloc
        );
    in
    lib.filter (x: x != null) (collectAddresses hostAllocation);

  # Build hostNames list for a machine
  buildHostNames =
    hostname: isLocal:
    let
      baseNames = [
        hostname
        "${hostname}.localdomain"
        "${hostname}.lynx-lizard.ts.net"
      ];
      ipAddresses = getHostIps hostname;
      localNames =
        if isLocal then
          [
            "127.0.0.1"
            "localhost"
          ]
        else
          [ ];
    in
    baseNames ++ ipAddresses ++ localNames;

  # Generate known hosts entries for all machines
  mkKnownHosts =
    currentHostname:
    let
      entries = lib.concatMap (
        machineName:
        let
          keys = machinesWithKeys.${machineName};
          isLocal = machineName == currentHostname;
          hostNames = buildHostNames machineName isLocal;
        in
        lib.mapAttrsToList (_keyFile: keyInfo: {
          name = "${machineName}-${keyInfo.keyType}";
          value = {
            hostNames = hostNames;
            publicKey = keyInfo.keyContent;
          };
        }) keys
      ) (lib.attrNames machinesWithKeys);
    in
    lib.listToAttrs entries;
in
{
  flake.nixosModules.ssh-known-hosts =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.ssh-known-hosts";

      imports = [
        self.nixosModules.inventory
      ];

      config = {
        programs.ssh.knownHosts = mkKnownHosts config.networking.hostName;
      };
    };
}
