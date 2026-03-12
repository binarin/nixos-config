{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.nix-store-edit = pkgs.writeShellApplication {
        name = "nix-store-edit";
        runtimeInputs = with pkgs; [
          coreutils
          findutils
        ];
        text = builtins.readFile ../../scripts/nix-store-edit.sh;
      };
    };
}
