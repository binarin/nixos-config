import imageio.v2 as iio2
import imageio.v3 as iio3
import numpy as np
import pytest


@pytest.mark.parametrize("plugin", ["pillow", "PNG-PIL"])
@pytest.mark.parametrize("dtype", ["<u2", ">u2"])
@pytest.mark.parametrize("strided", [False, True])
def test_uint16_png_values(tmp_path, plugin, dtype, strided):
    values = np.tile([0, 1, 255, 256, 32768, 65535], (6, 1)).astype(dtype)
    if strided:
        values = values[::-2, ::-1]
    path = tmp_path / "uint16.png"
    if plugin == "pillow":
        iio3.imwrite(path, values, plugin=plugin, mode="I;16", is_batch=False)
        actual = iio3.imread(path, plugin=plugin)
    else:
        iio2.imwrite(path, values, format=plugin, prefer_uint8=False)
        actual = iio2.imread(path, format=plugin)
    assert actual.dtype == np.uint16
    np.testing.assert_array_equal(actual, values)


@pytest.mark.parametrize("plugin", ["pillow", "PNG-PIL"])
@pytest.mark.parametrize("mode, channels", [("L", 0), ("RGB", 3), ("RGBA", 4)])
def test_uint8_png_modes(tmp_path, plugin, mode, channels):
    shape = (6, 8, channels) if channels else (6, 8)
    values = np.arange(np.prod(shape), dtype=np.uint8).reshape(shape)
    path = tmp_path / "uint8.png"
    if plugin == "pillow":
        iio3.imwrite(path, values, plugin=plugin, mode=mode)
        actual = iio3.imread(path, plugin=plugin)
    else:
        iio2.imwrite(path, values, format=plugin)
        actual = iio2.imread(path, format=plugin)
    assert actual.dtype == np.uint8
    np.testing.assert_array_equal(actual, values)
