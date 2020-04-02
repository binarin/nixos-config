{config, lib, ...}:

{
  options = {
    userPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
    };
  };
}
