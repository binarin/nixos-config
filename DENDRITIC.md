# How dendritic module looks like / conversion template

This is the most complete module, where every aspect is configured.

```nix
# Lives in a file `modules/thingy.nix`
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

# How dendritic nixos configuraion should look like

```nix
# Lives in a file `modules/machines/some-machine.nix`
{ self, inputs, config, ... }:
let
    inventoryHostName = "some-machine";
    system = "x86_64-linux";
in
{
  flake.deploy.nodes.some-machine = {
    hostname = config.inventory.ipAllocation."${inventoryHostName}".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.some-machine;
    };
  };

  flake.nixosConfigurations.some-machine = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      flake = {
        inherit self inputs config;
      };
      inherit inventoryHostName; # Can be used to include per-machine modules dynamically
    };

  modules = [
      self.nixosModules.some-machine-configuration
    ]
  };

  flake.nixosModules.some-machine-configuration = {config, lib, pkgs, ...}: {
    key = "nixos-config.nixos.some-machine-configuration";
    imports = [
        self.nixosModules.default
        ...
    ]
    ++ self.nixosSharedModules;

    ...
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

You should also check whether it makes sense to move flake inputs to `flake-file.inputs` here - from `inputs-old`.

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
- [x] **emacs** (nixos + home)
- [x] **hyprland** (nixos + home)
- [x] **sops** (nixos + home)
- [x] **vfio** (dropped - removed entirely)
- [x] **waybar** (nixos + home)
- [x] **wsl** (dropped - removed entirely)
- [x] **misc** (integrated into nix.nix, default-new.nix, home-manager.nix)
- [x] **host-config** (nixos + shared)
- [x] **impermanence** (nixos + home)

## Phase 2: NixOS-only modules (Priority: MEDIUM)

Single file conversions, simpler but still need proper dendritic structure.

- [x] bleeding.nix
- [x] eternal-terminal.nix
- [x] expose-local-http.nix
- [x] flake-packages.nix
- [x] home-manager.nix
- [x] impure-nix-setup.nix
- [x] keep-nix-build-sources.nix
- [x] large-console-fonts.nix
- [x] lxc.nix
- [x] pam-u2f.nix
- [x] secure-boot.nix
- [x] security.nix
- [x] server.nix
- [x] sshd.nix
- [x] standard-linux-tools.nix
- [x] tailscale.nix
- [x] trezor.nix
- [x] use-nix-cache.nix
- [x] vscode-remote-workaround.nix (dropped - unused)

## Phase 3: Home-only modules (Priority: MEDIUM)

Single file conversions.

- [x] cad.nix
- [x] fonts.nix
- [x] foot.nix
- [x] gc.nix
- [x] git.nix
- [x] interactive-cli.nix
- [x] kmonad.nix (dropped - trivial)
- [x] lnxlink.nix (dropped - unused)
- [x] wayland.nix
- [x] wezterm.nix

## Phase 4: Shared-only modules (Priority: LOW)

- [x] flake-files.nix
- [x] public-keys.nix
- [x] zenburn.nix

## Completed Conversions

- [x] **sops.nix** (nixos + home)
- [x] **stylix.nix** (nixos + home + shared)
- [x] **zenburn.nix** (shared only)
- [x] **home-manager.nix** (nixos only)
- [x] **firefox.nix** (nixos + home)
- [x] **bluetooth.nix** (nixos only)
- [x] **gui.nix** (nixos + home)
- [x] **move-xdg.nix** (dropped - inlined into ishamael configuration, furfur configuration removed)
- [x] **emacs.nix** (nixos + home)
- [x] **vfio.nix** (dropped - removed entirely)
- [x] **wsl.nix** (dropped - removed entirely)
- [x] **hyprland.nix** (nixos + home)
- [x] **impermanence.nix** (nixos + home)
- [x] **waybar.nix** (nixos + home)
- [x] **misc.nix** (integrated into nix.nix, default-new.nix, home-manager.nix)
- [x] **cad.nix** (home only)
- [x] **fonts.nix** (home only)
- [x] **foot.nix** (home only)
- [x] **gc.nix** (home only)
- [x] **git.nix** (home only)
- [x] **interactive-cli.nix** (home only)
- [x] **wayland.nix** (home only)
- [x] **wezterm.nix** (home only)
- [x] **host-config.nix** (nixos + shared)
- [x] **flake-files.nix** (shared only)
- [x] **public-keys.nix** (shared only)
- [x] All Phase 2 NixOS-only modules (bleeding, eternal-terminal, expose-local-http, flake-packages, impure-nix-setup, keep-nix-build-sources, large-console-fonts, lxc, pam-u2f, secure-boot, security, server, sshd, standard-linux-tools, tailscale, trezor, use-nix-cache)
