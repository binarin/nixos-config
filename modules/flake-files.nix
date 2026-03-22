{
  self,
  lib,
  ...
}:
let
  # == Why this module exists ==
  #
  # In a flake, `self` is a store path containing the entire repo source.
  # Any reference like `"${self}/files/foo"` creates a build-time dependency
  # on the whole source tree. When the git working tree is dirty (or any file
  # in the repo changes), `self` gets a new hash, which cascades through
  # every derivation that references it — even if the specific file didn't change.
  #
  # This causes deploy skip-if-unchanged checks to always see a "different"
  # system, because the nixos-system derivation hash changes with every
  # unrelated edit.
  #
  # == How builtins.path fixes it ==
  #
  # `builtins.path { path = ...; name = ...; }` copies a path into the store
  # as a new, independent entry whose hash depends ONLY on the content of that
  # specific file or directory — not on the enclosing store path (`self`).
  # This breaks the dependency chain: if the file/dir content hasn't changed,
  # the resulting store path is identical regardless of `self`'s hash.
  #
  # == Pitfalls ==
  #
  # 1. The `path` argument must be an actual Nix path type, not a string.
  #    String interpolation like `"${self}/files/foo"` produces a string with
  #    a store path reference, and Nix refuses to coerce it back to a path.
  #    We use `self.outPath + "/files/${name}"` (string concatenation on a path)
  #    which preserves the path type.
  #
  # 2. For directories, builtins.path copies the entire subtree, preserving
  #    file permissions (including executable bits). This makes it suitable
  #    for both files and directories, and eliminates the need for workarounds
  #    like the base64-encode-in-git / decode-at-build-time pattern.
  #
  # 3. builtins.readFile is still needed when callers want file *content* as
  #    a string (e.g., to embed in a larger script or config). builtins.path
  #    only gives you a store path.

  # Copy a file or directory from files/ into an independent store path.
  # The hash depends only on the content, not on `self`.
  isolate =
    name:
    builtins.path {
      path = self.outPath + "/files/${name}";
      name = lib.replaceStrings [ "/" ] [ "__" ] name;
    };

  # Same as isolate but for arbitrary paths relative to the repo root.
  isolate' =
    name:
    builtins.path {
      path = self.outPath + "/${name}";
      name = lib.replaceStrings [ "/" ] [ "__" ] name;
    };

  # Read file content as a string. This also breaks the self dependency
  # because builtins.readFile evaluates at eval time, converting the path
  # reference into a plain string value.
  read = name: builtins.readFile "${self}/files/${name}";

  read' = name: builtins.readFile "${self}/${name}";

  selfLib = {
    inherit read read';

    # Return an independent store path for a file in files/.
    file = isolate;

    # Return an independent store path for a script in files/.
    # builtins.path preserves the executable bit from git, so callers
    # must ensure the file is +x in the repo.
    script = isolate;

    # Return an independent store path for an arbitrary repo-relative path.
    file' = isolate';

    optionalFile' =
      name:
      let
        path = "${self}/${name}";
      in
      if builtins.pathExists path then
        isolate' name
      else
        builtins.toFile (lib.replaceStrings [ "/" ] [ "__" ] name) "";

    # Copy a directory from files/ into an independent store path.
    dir = isolate;
    dir' = isolate';
  };
in
{
  flake = {
    # flake.lib is typed as `raw` by flake-parts, which doesn't merge across modules.
    # Re-declare it as a mergeable attrset so multiple modules can contribute.
    options.lib = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = { };
    };

    # Exposed as a flake output so any module can use
    # self.lib.self.{file,dir,read,...} without importing a NixOS module.
    config.lib.self = selfLib;
  };
}
