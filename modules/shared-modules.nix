{ lib, config, ... }:
{
  options = {
    nixosSharedModules = lib.mkOption {
      description = "NixOS modules that should be injected into every configuration (the same as home-manager.sharedModules, but on flake level)";
      type = with lib.types; listOf deferredModule;
      default = [ ];
    };
  };

  config = {
    flake.nixosSharedModules = config.nixosSharedModules;
  };
}
