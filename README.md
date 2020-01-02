Building a specific machine
===========================

After checking out submodules, any configuration can be built using command like

    nix-build '<nixpkgs/nixos>' -A system \
        -I nixpkgs=$(pwd)/nixpkgs \
        -I nixos-config=$(pwd)/configuration.nix-ishamael \
        -I nixpkgs-overlays=$(pwd)/overlay \
        --keep-going
        
    ./result/bin/switch-to-configuration boot
        
This command is used for bootstrapping or CI. 
 
But on every machine where I update things manually, I usually put a symlink from `configuration.nix-XXXX` to to `configuration.nix` (it's git-ignored, so doesn't get into the way). Then after bootstrapping by using the above commands, I can simply use `nixos-rebuild`, as custom `NIX_PATH` is [already set to my custom values](packages/use-my-overlays.nix).
        
        
Highlights
==========
 
Multiple pinned versions of `nixpkgs`
-------------------------------------
 
There are multiple submodules pointing to different `nixpkgs` commits. The primary one is in `nixpkgs` directory, and it's used for defining the system (for me it's usually points to a stable release).

And some packages are being installed from a `nixpkgs-master`, which usually points to a more recent commit. This is achieved by an [overlay](overlay/02-bleeding.nix), which injects a whole nixpkgs under an attribute. Then in can be used both in configuration files, by referring to packages like `pkgs.bleeding.telegram`, and even from CLI: `nix-env -iA nixos.bleeding.telegram`.

Overlays
--------

Overlays are automatically loaded from `overlay` folder.

For some reason `nixos-rebuild` was ignoring overlays from `NIX_PATH`, so it was necessary to [populate `nixpkgs.overlays` option manually](packages/use-my-overlays.nix)

Home-manager
------------

[home-manager](https://github.com/rycee/home-manager) also lives in a submodule, and it's easy to use just by [loading its NixOS module](users/binarin.nix).
