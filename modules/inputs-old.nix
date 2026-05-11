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

    nixpkgs.url = "github:nixos/nixpkgs";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";

    import-tree.url = "github:vic/import-tree";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs-unstable";

    emacs-overlay.url = "emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";

    pre-commit-hooks.url = "github:cachix/git-hooks.nix";

    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    arion.url = "github:hercules-ci/arion";
    arion.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };
}
