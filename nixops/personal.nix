{
  network.description = "personal servers";
  kodi = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-kodi
     ];
  };
  amon = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-amon
     ];
  };
}
