{
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    arion = {
      url = "github:hercules-ci/arion";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };
    clan-core = {
      url = "git+https://git.clan.lol/clan/clan-core";
      inputs = {
        disko.follows = "disko";
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
        sops-nix.follows = "sops-nix";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    common.url = "git+https://forgejo.lynx-lizard.ts.net/binarin/swan-lake-common?ref=swan-lake-current";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    direnv-instant = {
      url = "github:Mic92/direnv-instant";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    disko.follows = "common/disko";
    emacs-niri-awareness = {
      url = "github:binarin/emacs-niri-awareness";
      flake = false;
    };
    emacs-overlay.follows = "common/emacs-overlay";
    emacs-tramp-rpc.follows = "common/emacs-tramp-rpc";
    flake-compat.url = "github:NixOS/flake-compat";
    flake-parts.follows = "common/flake-parts";
    home-manager.follows = "common/home-manager";
    impermanence.url = "github:nix-community/impermanence";
    import-tree.follows = "common/import-tree";
    lan-mouse.url = "github:feschber/lan-mouse";
    nci = {
      url = "github:90-008/nix-cargo-integration";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    niks3 = {
      url = "github:Mic92/niks3";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    nix-ai-tools = {
      url = "github:numtide/llm-agents.nix";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };
    nix-bindings-rust = {
      url = "github:nixops4/nix-bindings-rust";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixgl";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixos-raspberrypi = {
      url = "github:nvmd/nixos-raspberrypi/develop";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.follows = "common/nixpkgs";
    nixpkgs-unstable.follows = "common/nixpkgs-unstable";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    sops-nix.follows = "common/sops-nix";
    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix = {
      url = "github:nix-community/stylix/release-26.05";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };
    system-manager = {
      url = "github:numtide/system-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix.url = "github:numtide/treefmt-nix";
    wprintidle-c = {
      url = "github:devrintalen/wprintidle-c";
      flake = false;
    };
  };
}
