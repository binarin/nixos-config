{ flake, pkgs, lib, system, config, nixosConfig, ... }:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{
  imports = [
    self.homeModules.default
  ];

  config = {
    impermanence.persist-bind-directories-no-root = lib.mkIf config.hostConfig.feature.workstation [
      "personal-workspace"
    ];
    programs.git = {
      userName = "Alexey Lebedeff";
      userEmail = "binarin@binarin.info";
    };
  };
}
