{lib, ...}:
{
  imports = [
    ./valak
  ];
  hostConfig.feature.fast-rebuild = lib.mkForce true;
}
