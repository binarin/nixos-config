{ inputs, ... }:
{
  perSystem =
    {
      self',
      pkgs,
      lib,
      ...
    }:
    {
      # For 'nix fmt'
      formatter = pkgs.writeScriptBin "nixfmt-all" ''
        #!${lib.getExe pkgs.bash}
        ${lib.getExe' pkgs.nixfmt-rfc-style "nixfmt"} **/*.nix
      '';
    };
}
