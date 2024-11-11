{ inputs, ... }:

let
  self = inputs.self;
in
{
  flake = {
    lib.hostConfig = {
      ipamInterfaces = host: (builtins.fromJSON (builtins.readFile "${self}/ipam.json"))."${host}".interfaces;
      userSopsFile = inventoryHostName: userName: "${self}/secrets/${inventoryHostName}/user-${userName}.yaml";
      hostSopsFile = inventoryHostName: "${self}/secrets/${inventoryHostName}/secrets.yaml";
    };
  };
}
