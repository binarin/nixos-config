# How dendritic module looks like / conversion template

This is the most complete module, where every aspect is configured.

```nix
# Lives in a file `thingy.nix`
{self, inputs, lib, ...}: {
    # additional flake inputs, should be materialized with `nix run '.#write-flake'`.
    # Important: if `imports` below is used, this should be done in two stages:
    # - at first, only `flake-file.inputs` should be added and `nix run '.#write-flake'` should be done
    # - only then `imports` should be added
    flake-file.inputs = {
        some-flake.url = "github:some/repo";
    };

    imports = [
        inputs.some-flake.flakeModule
    ];

    nixosSharedModules = [
        self.nixosModules.thingy
    ];

    flake.modules.generic.thingy = {...}: {
        key = "nixos-config.modules.home.thingy";
    };

    flake.nixosModules.thingy = {config, lib, pkgs, ...}: {
        key = "nixos-config.modules.nixos.thingy";
        imports = [
            self.modules.generic.thingy
        ];
        home-manager.sharedModules = [
            self.homeModules.thingy
            self.modules.generic.thingy
        ];
    };

    flake.homeModules.thingy = {config, lib, pkgs, ...}: {
        key = "nixos-config.modules.home.thingy";
        imports = [
            inputs.some-repo.homeModules.default
        ];
    };


}
```

# Conversion process for a single aspect

Make sure that all existing changes are committed to git, run `just
lint` and commit again if it reformats anything.

We are going to convert one aspect at a time (same filename in
modules/nixos, modules/home and modules/shared; `default.nix` should
be ignored). Start an aspect conversion only when user explicitly
requests it, and it's listed in the plan below.

Not yet converted modules can refer to `flake` argument, to get
`inputs` and then `self` from there - this is not needed, both `self`
and `inputs` can be brought in scope through flake-module (as it
happens in the example above).

With `thingy` as example: - `modules/nixos/thingy.nix` goes into
`flake.nixosModules.thingy`, and should be added to
`nixosSharedModules` also. - `modules/home/thingy.nix` goes into `flake.homeModules.thingy`,
and should be also added to `home-manager.sharedModules` in
nixos module (if `thingy` doesn't have nixos module, introduce
one with only `home-manager.sharedModules`). - `modules/shared/thingy.nix` goes into `flake.modules.generic.thingy`.

Unused parts from the template above should be removed.

Every converted aspect should also include `key`, as shown in the example.

One thing to be careful about is presence of `config` and/or `options`
attributes in the original, non-converted modules - it changes
configuration handling a bit. In that case your own additions
(e.g. `home-manager.sharedModules` assignment should go inside
`config` attribute of that aspect.

The sources used for conversion should be removed from git (not
committed yet), to prevent autoload mechanism from finding them. And
newly created module should also be immediately git-staged (shouldn't
be up to date, just make filename known to flakes machinery).

Start with doing `just nixOpts= eval-nixos` (non-verbose mode), which will evaluate a single
configuration for the current machine. Do this in a loop, until fixed.

At this stage, `just eval-all` should run (it runs for quite a long
time, so if running with a timeout - set it to 30 minutes). Do it in a
loop, running `eval-all` and going back to `just eval-nixos` step for
the failed configuration (if any).

Now you can create a git commit with this changes. Now we should run
`just lint` in a loop - it can just re-format changed files, or maybe
some changes should be done by you. When everyting is clean, amend the
last git commit with formatting changes.

Mark the conversion as done.

If you have made some discoveries/observations during this conversion
that can be useful for subsequent conversions, also update this file,
and do a separate git commit for this.

# Plan

## DONE Convert firefox.nix

**Complexity: LOW-MEDIUM**

**Files to convert:**

- `modules/nixos/firefox.nix` (30 lines) - Firefox policies and language packs
- `modules/home/firefox.nix` (75 lines) - User profiles, settings, and CSS customization
- `modules/shared/firefox.nix` - Does NOT exist (no generic module needed)

**Flake inputs:** None

**Target structure:**

- Create `modules/flake-parts/firefox.nix`
- Content from `modules/nixos/firefox.nix` → `flake.nixosModules.firefox`
- Content from `modules/home/firefox.nix` → `flake.homeModules.firefox`
- Add `self.nixosModules.firefox` to `nixosSharedModules`
- Add `self.homeModules.firefox` to `home-manager.sharedModules` in nixos module

**Special considerations:**

- Home module uses `config.lib.self.read "firefox-userChrome.css"` (defined in `modules/shared/flake-files.nix`) - should continue working via autowiring
- External file dependency: `files/firefox-userChrome.css` - no changes needed
- Both modules conditional on `config.hostConfig.feature.gui`
- Module is referenced in `modules/home/stylix.nix` and `modules/home/hyprland.nix` (no changes needed there)
