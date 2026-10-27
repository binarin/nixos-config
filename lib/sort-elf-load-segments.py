"""Sort ELF64 little-endian PT_LOAD headers without moving segment contents.

Older Linux kernels derive AT_PHDR from the first PT_LOAD header. Bun's
standalone binaries can have that header out of virtual-address order.
Only PT_LOAD slots are rewritten; all other headers and file bytes stay put.
"""

import struct
import sys


def sort_load_segments(path):
    with open(path, "r+b") as elf:
        header = elf.read(64)
        if len(header) != 64 or header[:6] != b"\x7fELF\x02\x01":
            raise ValueError("expected a little-endian ELF64 binary")
        phoff = struct.unpack_from("<Q", header, 32)[0]
        phentsize, phnum = struct.unpack_from("<HH", header, 54)
        if phentsize != 56 or phnum == 0xFFFF:
            raise ValueError("unsupported program header table")
        elf.seek(phoff)
        table = elf.read(phentsize * phnum)
        if len(table) != phentsize * phnum:
            raise ValueError("truncated program header table")
        entries = [table[i:i + phentsize] for i in range(0, len(table), phentsize)]
        slots = [i for i, entry in enumerate(entries) if struct.unpack_from("<I", entry)[0] == 1]
        if not slots:
            raise ValueError("no PT_LOAD headers")
        loads = sorted((entries[i] for i in slots), key=lambda entry: struct.unpack_from("<Q", entry, 16)[0])
        for index, entry in zip(slots, loads):
            elf.seek(phoff + index * phentsize)
            elf.write(entry)
        print(f"Sorted {len(slots)} PT_LOAD headers in {path}")


if __name__ == "__main__":
    sort_load_segments(sys.argv[1])
