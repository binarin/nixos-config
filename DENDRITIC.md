# Dendritic Conversion Process

This document describes the conversion process for migrating modules to the dendritic pattern.
It is kept for reference only - the actual conversion is complete.

## Conversion process for a single aspect

Make sure that all existing changes are committed to git, run `just
lint` and commit again if it reformats anything.

We are going to convert one aspect at a time (same filename in
modules/nixos, modules/home and modules/shared; `default.nix` should
be ignored). Start an aspect conversion only when user explicitly
requests it.

Not yet converted modules can refer to `flake` argument, to get
`inputs` and then `self` from there - this is not needed, both `self`
and `inputs` can be brought in scope through flake-module (as it
happens in the dendritic pattern).

With `thingy` as example:
- `modules/nixos/thingy.nix` goes into `flake.nixosModules.thingy`, and should be added to `nixosSharedModules` also.
- `modules/home/thingy.nix` goes into `flake.homeModules.thingy`, and should be also added to `home-manager.sharedModules` in nixos module (if `thingy` doesn't have nixos module, introduce one with only `home-manager.sharedModules`).
- `modules/shared/thingy.nix` goes into `flake.modules.generic.thingy`.

You should also check whether it makes sense to move flake inputs to `flake-file.inputs` here - from `inputs-old`.

Unused parts from the template should be removed.

Every converted aspect should also include `key`, as shown in the example.

If all modules of the aspect are being gated on one or more `hostConfig.feature`-s:
- remove this gating altogether
- remove aspect from `nixosSharedModules`
- explicitly import it into relevant nixos configurations - in `modules/flake-parts/machines/` if present, and in `configurations/nixos/MACHINE/configuration.nix` otherwise.

One thing to be careful about is presence of `config` and/or `options`
attributes in the original, non-converted modules - it changes
configuration handling a bit. In that case your own additions
(e.g. `home-manager.sharedModules` assignment should go inside
`config` attribute of that aspect.

The sources used for conversion should be removed from git (not
committed yet), to prevent autoload mechanism from finding them. And
newly created module should also be immediately git-staged (shouldn't
be up to date, just make filename known to flakes machinery).

Start with doing `just nixOpts= eval-nixos` (non-verbose mode), which
will evaluate a single configuration for the current machine. Do this
in a loop, until fixed.

At this stage, `just eval-all` should run (it runs for quite a long
time, so if running with a timeout - set it to 30 minutes). Do it in a
loop, running `eval-all` and going back to `just eval-nixos` step for
the failed configuration (if any).

Now you can create a git commit with this changes. Now we should run
`just lint` in a loop - it can just re-format changed files, or maybe
some changes should be done by you. When everything is clean, amend the
last git commit with formatting changes.

Mark the conversion as done.

If you have made some discoveries/observations during this conversion
that can be useful for subsequent conversions, also update this file,
and do a separate git commit for this.
