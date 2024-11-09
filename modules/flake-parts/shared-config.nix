{inputs, ...}:

let
  self = inputs.self;
in
{
  flake = {
    sharedModules.hostConfig = "${self}/modules/host-config/default.nix";
    lib.hostConfig = {
      forHost = inventoryHostName: (import "${self}/modules/host-config/${inventoryHostName}.nix") // {};
      userSopsFile = inventoryHostName: userName: "${self}/secrets/${inventoryHostName}/user-${userName}.yaml";
      hostSopsFile = inventoryHostName: "${self}/secrets/${inventoryHostName}/secrets.yaml";
    };
  };
}
