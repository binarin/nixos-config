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

  # For each machine, find SSH host public keys from secrets/
  secretsKeys = lib.mapAttrs (
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
      }
    ) sshKeyFiles
  ) machineDirs;

  # Read clan vars directory to find machines with openssh host keys
  varsDir = "${self}/vars/per-machine";
  varsDirContents = if builtins.pathExists varsDir then builtins.readDir varsDir else { };
  varsMachineDirs = lib.filterAttrs (_name: type: type == "directory") varsDirContents;

  # For each clan machine, find SSH host public keys from vars/per-machine/*/openssh/
  clanKeys = lib.mapAttrs (
    machineName: _:
    let
      opensshDir = "${varsDir}/${machineName}/openssh";
      hasOpensshDir = builtins.pathExists opensshDir;
      dirContents = if hasOpensshDir then builtins.readDir opensshDir else { };
      sshKeyDirs = lib.filterAttrs (
        name: type: type == "directory" && lib.hasPrefix "ssh.id_" name && lib.hasSuffix ".pub" name
      ) dirContents;
    in
    lib.mapAttrs (
      keyDir: _:
      let
        keyContent = lib.trim (builtins.readFile "${opensshDir}/${keyDir}/value");
        # Extract key type from dirname: ssh.id_ed25519.pub -> ed25519
        keyType = lib.removePrefix "ssh.id_" (lib.removeSuffix ".pub" keyDir);
      in
      {
        inherit keyType keyContent;
      }
    ) sshKeyDirs
  ) varsMachineDirs;

  # Merge keys from both sources, clan keys take precedence
  allMachineNames = lib.unique (lib.attrNames machineDirs ++ lib.attrNames varsMachineDirs);
  machineKeys = lib.genAttrs allMachineNames (
    name:
    (secretsKeys.${name} or { }) // (clanKeys.${name} or { })
  );

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
