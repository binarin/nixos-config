{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.check-arion-images = pkgs.writeShellApplication {
        name = "check-arion-images";
        runtimeInputs = with pkgs; [
          jq
          curl
          findutils
          gnused
          coreutils
        ];
        text = builtins.readFile ../../scripts/check-arion-images.sh;
      };
    };
}
