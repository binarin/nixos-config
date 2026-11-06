{ inputs, lib, ... }:
{
  imports = [
    inputs.flake-file.flakeModules.dendritic
  ];

  systems = [
    "x86_64-linux"
    "aarch64-linux"
  ];

  flake-file.outputs = lib.mkForce ''
    inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules)
  '';

  flake-file.inputs = {
    flake-file.url = lib.mkForce "github:vic/flake-file";

    common.url = "git+https://forgejo.lynx-lizard.ts.net/binarin/swan-lake-common?ref=swan-lake-current";

    nixpkgs.url = lib.mkForce "";
    nixpkgs.follows = "common/nixpkgs";
    nixpkgs-unstable.follows = "common/nixpkgs-unstable";

    import-tree.url = lib.mkForce "";
    import-tree.follows = "common/import-tree";

    disko.follows = "common/disko";

    pre-commit-hooks.url = "github:cachix/git-hooks.nix";

    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    arion.url = "github:hercules-ci/arion";
    arion.inputs.nixpkgs.follows = "nixpkgs";
    arion.inputs.flake-parts.follows = "flake-parts";

    sops-nix.follows = "common/sops-nix";

    flake-parts.url = lib.mkForce "";
    flake-parts.inputs = lib.mkForce { };
    flake-parts.follows = "common/flake-parts";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };
}
