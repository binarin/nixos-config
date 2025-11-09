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

If all modules of the aspect are being gated on one or more `hostConfig.feature`-s: - remove this gating altogether - remove aspect from `nixosSharedModules` - explicitly import it into relevant nixos configuarions - in
`modules/flake-parts/machines/` if present, and in
`configurations/nixos/MACHINE/configuration.nix` otherwise.

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
some changes should be done by you. When everyting is clean, amend the
last git commit with formatting changes.

Mark the conversion as done.

If you have made some discoveries/observations during this conversion
that can be useful for subsequent conversions, also update this file,
and do a separate git commit for this.

# Plan

## Phase 1: Multi-directory aspects (Priority: HIGH)

These require merging 2-3 files and represent the core benefit of dendritic pattern.

- [x] **stylix** (nixos + home + shared) - 3-way merge
- [ ] **emacs** (nixos + home)
- [ ] **hyprland** (nixos + home)
- [ ] **sops** (nixos + home)
- [ ] **vfio** (nixos + home)
- [ ] **waybar** (nixos + home)
- [ ] **wsl** (nixos + home)
- [ ] **misc** (nixos + home)
- [ ] **move-xdg** (nixos + home)
- [ ] **host-config** (nixos + shared)
- [ ] **impermanence** (nixos + home) - Complete migration of impermanence-new.nix

## Phase 2: NixOS-only modules (Priority: MEDIUM)

Single file conversions, simpler but still need proper dendritic structure.

- [ ] bleeding.nix
- [ ] eternal-terminal.nix
- [ ] expose-local-http.nix
- [ ] flake-packages.nix
- [ ] home-manager.nix
- [ ] impure-nix-setup.nix
- [ ] keep-nix-build-sources.nix
- [ ] large-console-fonts.nix
- [ ] lxc.nix
- [ ] pam-u2f.nix
- [ ] secure-boot.nix
- [ ] security.nix
- [ ] server.nix
- [ ] sshd.nix
- [ ] standard-linux-tools.nix
- [ ] tailscale.nix
- [ ] trezor.nix
- [ ] use-nix-cache.nix
- [ ] vscode-remote-workaround.nix

## Phase 3: Home-only modules (Priority: MEDIUM)

Single file conversions.

- [ ] cad.nix
- [ ] fonts.nix
- [ ] foot.nix
- [ ] gc.nix
- [ ] git.nix
- [ ] interactive-cli.nix
- [ ] kmonad.nix
- [ ] lnxlink.nix
- [ ] wayland.nix
- [ ] wezterm.nix

## Phase 4: Shared-only modules (Priority: LOW)

- [ ] flake-files.nix
- [ ] public-keys.nix
- [ ] zenburn.nix

## Completed Conversions

- [x] **stylix.nix** (nixos + home + shared)
- [x] **firefox.nix** (nixos + home)
- [x] **bluetooth.nix** (nixos only)
- [x] **gui.nix** (nixos + home)
