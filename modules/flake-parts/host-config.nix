{inputs, ...}:

let
  self = inputs.self;
in
{
  flake = {
    sharedModules.hostConfig = "${self}/modules/host-config/default.nix";
    lib.hostConfig = {
      ipamInterfaces = host: (builtins.fromJSON (builtins.readFile "${self}/ipam.json"))."${host}".interfaces;
      userSopsFile = inventoryHostName: userName: "${self}/secrets/${inventoryHostName}/user-${userName}.yaml";
      hostSopsFile = inventoryHostName: "${self}/secrets/${inventoryHostName}/secrets.yaml";
    };
  };
}
