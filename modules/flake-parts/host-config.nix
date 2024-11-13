{ inputs, ... }:

let
  self = inputs.self;
in
{
  flake = {
    lib.hostConfig = {
      userSopsFile = inventoryHostName: userName: "${self}/secrets/${inventoryHostName}/user-${userName}.yaml";
      hostSopsFile = inventoryHostName: "${self}/secrets/${inventoryHostName}/secrets.yaml";
    };
  };
}
