# NixOS configs

Configuration for multiple machines
-----------------------------------

Individual machine configurations live in a `configuration.nix-XXXXX` files. Plain `configuration.nix` is git-ignored, but on a machines where I do manual updates, it's usually created as a symlink to that machine specific config.

I've tried to loosely follow puppet [role/profile pattern](https://puppet.com/docs/pe/2018.1/the_roles_and_profiles_method.html). 
E.g. one of my [laptops configs](./configuration.nix-balthamel) just includes a bunch of other modules, including a [`workstation` profile](profile/workstation.nix) that I use for all my workstation.

After checking out submodules, any configuration can be built using command like

    nix-build '<nixpkgs/nixos>' -A system \
        -I nixpkgs=$(pwd)/nixpkgs \
        -I nixos-config=$(pwd)/configuration.nix-ishamael \
        -I nixpkgs-overlays=$(pwd)/overlay \
        --keep-going
        
    ./result/bin/switch-to-configuration boot
        
This command is used for bootstrapping or CI. 

After bootstrapping and making a forementioned symlink, I can simply use `nixos-rebuild`, as custom `NIX_PATH` is [already set to my custom values](packages/use-my-overlays.nix).


Multiple pinned versions of `nixpkgs`
-------------------------------------
 
There are multiple submodules pointing to different `nixpkgs` commits. The primary one is in `nixpkgs` directory, and it's used for defining the system (for me it's usually points to a stable release).

And some packages are being installed from a `nixpkgs-master`, which usually points to a more recent commit. This is achieved by an [overlay](overlay/02-bleeding.nix), which injects a whole nixpkgs under an attribute. Then in can be used both in configuration files, by referring to packages like `pkgs.bleeding.telegram`, and even from CLI: `nix-env -iA nixos.bleeding.telegram`.

Overlays
--------

Overlays are automatically loaded from `overlay` folder.

For some reason `nixos-rebuild` was ignoring overlays from `NIX_PATH`, so it was necessary to [populate `nixpkgs.overlays` option manually](packages/use-my-overlays.nix)

User management and home-manager
--------------------------------

[home-manager](https://github.com/rycee/home-manager) also lives in a submodule, and it's easy to use just by [loading its NixOS module](users/binarin.nix).

I don't manage passwords with NixOS, as I don't want even cached passwords to live in nix store. That allows me e.g. to simply export any store as a cache without authorization. The cost is the one-time manual password set-up during a new machine installation.

Updating nixpkgs
----------------

To get all the benefits of caching, `nixpkgs` are following [NixOS/nixpkgs-channels repo](https://github.com/NixOS/nixpkgs-channels). Updating it looks like this:

    cd nixpkgs
    git remote add channels https://github.com/NixOS/nixpkgs-channels
    git fetch channels
    git reset --hard channels/nixos-19.09
    
And `nixpkgs-master` I just bump to any fresh random commit, knowing that there may be no cache:

    cd nixpkgs-master
    git fetch origin master
    git reset --hard origin/master



