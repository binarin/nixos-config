{ ... }:
let
  version = "0.3.0";

  packageFn =
    {
      buildGoModule,
      fetchFromGitHub,
      lib,
    }:
    buildGoModule {
      pname = "immich-stack";
      inherit version;

      src = fetchFromGitHub {
        owner = "Majorfi";
        repo = "immich-stack";
        rev = "v${version}";
        hash = "sha256-NJt/q2g+fqO3O7pTPrb++QUdoE3INwDfa+cvSpoMv5s=";
      };

      # Upstream `go.mod` sits at the repo root; the `main` package lives in cmd/.
      subPackages = [ "cmd" ];

      # buildGoModule names the output binary after the subPackage path ("cmd").
      # Rename to the upstream program name so the wrapper/bin path is stable.
      postInstall = ''
        mv $out/bin/cmd $out/bin/immich-stack
      '';

      vendorHash = null;

      ldflags = [
        "-s"
        "-w"
      ];

      meta = {
        homepage = "https://github.com/Majorfi/immich-stack";
        description = "Automatically groups similar photos into stacks within Immich";
        license = lib.licenses.mit;
        mainProgram = "immich-stack";
        platforms = lib.platforms.linux;
      };
    };
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.immich-stack = pkgs.callPackage packageFn { };
    };

  flake.overlays.immich-stack = final: prev: {
    immich-stack = final.callPackage packageFn { };
  };
}
