{
  self,
  inputs,
  lib,
  config,
  ...
}:
let
  # Combine mapAttrs' and filterAttrs
  #
  # f can return null if the attribute should be filtered out.
  mapAttrsMaybe =
    f: attrs:
    lib.pipe attrs [
      (lib.mapAttrsToList f)
      (builtins.filter (x: x != null))
      builtins.listToAttrs
    ];
  forAllNixFiles =
    dir: f:
    if builtins.pathExists dir then
      lib.pipe dir [
        builtins.readDir
        (mapAttrsMaybe (
          fn: type:
          if type == "regular" then
            let
              name = lib.removeSuffix ".nix" fn;
            in
            lib.nameValuePair name (f "${dir}/${fn}")
          else if type == "directory" && builtins.pathExists "${dir}/${fn}/default.nix" then
            lib.nameValuePair fn (f "${dir}/${fn}")
          else
            null
        ))
      ]
    else
      { };

  specialArgs = {
    flake = {
      inherit self inputs config;
    };
  };

  mkLinuxSystem =
    mod:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = specialArgs // {
        hostConfig = {
          isLinux = true;
        };
      };
      modules = [ mod ] ++ config.nixosSharedModules;
    };
in
{
  config = {
    flake = {
      nixosConfigurations = forAllNixFiles "${self}/configurations/nixos" (fn: mkLinuxSystem fn);

      nixosModules = forAllNixFiles "${self}/modules/nixos" (fn: fn);

      homeModules = forAllNixFiles "${self}/modules/home" (fn: fn);

      overlays = forAllNixFiles "${self}/overlays" (fn: import fn specialArgs);

      sharedModules = forAllNixFiles "${self}/modules/shared" (fn: fn);

      helpers = forAllNixFiles "${self}/modules/helpers" (fn: import fn { inherit self lib; });
    };
  };
}
