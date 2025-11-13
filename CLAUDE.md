This is my personal infra:

- NixOS configurations
- Ansible for non-NixOS machines (but some data is being consumed from NixOS configurations)
- Terraform for creating non-NixOS machines

# Git

Before starting doing any changes, check that everything is
committed. If not, analyze current uncommitted changes, and do one or
more commits, trying to group logically related changes.

When commiting changes, check whether there are any commits not pushed
to upstream yet, and if the current change is actually should be a
part of one of these commits - in that case create a fixup commit.

# NixOS

When new .nix file created, it should be immediately added to git
(even in empty state), as flakes only see committed or staged files
(but there is no reason to stage/commit changes after changes were
made to an existing file).

## Dendritic

This repo follows dendritic pattern: where all classes of modules for
the same aspect live in the same file (i.e. `modules/something.nix`
instead of `modules/{home,nixos,shared}/something.nix`). Here
`home`/`nixos`/`shared` are class, and `something` is aspect.

Flake inputs are also defined in .nix modules,
(`flake-file.inputs.XXX.url = `) and the final flake.nix is generated
by `nix run '.#write-flake'`.

Having `key` in every module is of utmost importance, as it's being
used for the deduplication by the module system.

### Dendritic module template

This is the most complete module, where every aspect is
configured. One simplification would be to omit introducing extra
level with `config = {` if there is no `options` defined - this is an
officially supported shortcut.

```nix
# Lives in a file `modules/thingy.nix`
{self, inputs, lib, ...}: {
  imports = [
    inputs.some-flake.flakeModule
  ];

  config = {
    # additional flake inputs, should be materialized with `nix run '.#write-flake'`.
    # Important: if `imports` below is used, this should be done in two stages:
    # - at first, only `flake-file.inputs` should be added and `nix run '.#write-flake'` should be done
    # - only then `imports` should be added
    flake-file.inputs = {
      some-flake.url = "github:some/repo";
    };

    flake.modules.generic.thingy = {...}: {
      key = "nixos-config.modules.home.thingy";
      options = { ... };
      config = { ... };
    };

    flake.nixosModules.thingy = {config, lib, pkgs, ...}: {
      key = "nixos-config.modules.nixos.thingy";
      imports = [
        self.modules.generic.thingy
      ];
      options = { ... };
      config = { ... };
    };

    flake.homeModules.thingy = {config, lib, pkgs, ...}: {
      key = "nixos-config.modules.home.thingy";
      imports = [
        inputs.some-repo.homeModules.default
      ];
      options = { ... };
      config = { ... };
    };
  };
}
```

### Dendritic nixos configuration template

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

### Validating changes

When making changes to dendritic modules:

1. Start with `just nixOpts= eval-nixos` (non-verbose mode), which evaluates a single configuration for the current machine. Do this in a loop until fixed.

2. Run `just eval-all` (runs for quite a long time, set timeout to 30 minutes if needed). Do it in a loop, going back to `just eval-nixos` for any failed configuration.

3. Run `just lint` in a loop - it can reformat changed files. When everything is clean, amend the last git commit with formatting changes if needed.

## Inventory

NixOS configuration is primary source of inventory, in inventory/ -
main parts are SSH public keys and IP allocation. Ideally, it should
be the only source of truth - but for now there are some IP addresses
hardcoded here and there.

# Ansible

# Terraform

# SSH security
