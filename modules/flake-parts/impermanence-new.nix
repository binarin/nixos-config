{...}: {
  flake.nixosModules.impermanence-new = {lib, config, ...}: {
    key = "nixos-config.impermanenc-new";
    options = {
      impermanence.enable = lib.mkEnableOption "Enable impermanence";
    };
    config = {
      impermanence.enable = lib.mkDefault config.hostConfig.feature.impermanence;
    };
  };
}
