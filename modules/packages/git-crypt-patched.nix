let
  packageFn =
    { git-crypt, fetchpatch }:
    git-crypt.overrideAttrs (_prev: {
      pname = "git-crypt-patched";
      # https://github.com/AGWA/git-crypt/pull/222 — not yet merged (since 2021)
      patches = [
        (fetchpatch {
          url = "https://github.com/AGWA/git-crypt/commit/2da5e0016e53aba381046063c24c07f1bee3d824.diff";
          sha256 = "sha256-fyHS2oeElUh+KEtvfnpf2/IiJPNSu03af+ilYFm3wOU";
        })
      ];
    });
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.git-crypt-patched = pkgs.callPackage packageFn { };
    };

  flake.overlays.git-crypt-patched = final: prev: {
    git-crypt-patched = final.callPackage packageFn { };
  };
}
