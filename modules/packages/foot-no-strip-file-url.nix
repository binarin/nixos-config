{ lib, ... }:
{
  flake.overlays.foot-no-strip-file-url = final: prev: {
    foot = prev.foot.overrideAttrs (old: {
      patches = (old.patches or [ ]) ++ [ ../../files/patches/foot-no-strip-file-url.patch ];
    });
  };
}
