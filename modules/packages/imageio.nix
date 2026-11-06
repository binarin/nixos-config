{ ... }:
{
  flake.overlays.imageio-pillow = final: prev: {
    pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
      (pythonFinal: pythonPrev: {
        imageio = pythonPrev.imageio.overridePythonAttrs (old: {
          patches = (old.patches or [ ]) ++ [ ../../files/patches/imageio-pillow-uint16.patch ];
          postPatch = (old.postPatch or "") + ''
            cp ${../../tests/test_imageio_pillow_uint16.py} tests/test_pillow_uint16.py
          '';
        });
      })
    ];
  };
}
