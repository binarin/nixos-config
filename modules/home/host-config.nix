{flake, ...}:
{
  imports = [
    flake.inputs.self.sharedModules.hostConfig
  ];
}
