{
  inputs = {
    # nixpkgs.url = "path:/home/binarin/personal-workspace/nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";

    mac-app-util.url = "github:hraban/mac-app-util";
    mac-app-util.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin-master.url = "github:LnL7/nix-darwin";
    nix-darwin-master.inputs.nixpkgs.follows = "nixpkgs";

    # home-manager.url = "path:/home/binarin/personal-workspace/home-manager";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs-unstable";

    impermanence.url = "github:nix-community/impermanence";

    lanzaboote.url = "github:nix-community/lanzaboote";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs-unstable";
    lanzaboote.inputs.flake-parts.follows = "flake-parts";
    # lanzaboote.inputs.pre-commit-hooks-nix.follows = "";

    emacs-overlay.url = "emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";

    hyprland = {
      url = "https://github.com/hyprwm/Hyprland";
      ref = "refs/tags/v0.49.0";
      type = "git";
      submodules = true;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };

    # XXX Only to override the one in hyprland, until nested overrides are possible
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs-unstable";

    waybar.url = "github:Alexays/Waybar";
    waybar.inputs.nixpkgs.follows = "nixpkgs-unstable";

    hyprland-contrib.url = "github:hyprwm/contrib";
    hyprland-contrib.inputs.nixpkgs.follows = "nixpkgs";

    # Software inputs
    stylix.url = "github:danth/stylix/release-25.05";
    stylix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    caddy-cloudflare.url = "github:binarin/caddy-with-plugins";
    caddy-cloudflare.inputs.nixpkgs.follows = "nixpkgs"; # golang < 1.23 - see https://github.com/nix-community/gomod2nix/issues/117

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

  outputs =
    inputs:
    let
      systems = [ "x86_64-linux" "aarch64-darwin" ];
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;
      imports = [
        ./modules/flake-parts/autowire.nix
        ./modules/flake-parts/deploy.nix
        ./modules/flake-parts/devshell.nix
        ./modules/flake-parts/formatter.nix
        ./modules/flake-parts/ci-template.nix
      ];
    };
}
