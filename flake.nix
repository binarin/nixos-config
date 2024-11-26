{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    hyprland = {
      url = "https://github.com/hyprwm/Hyprland";
      ref = "refs/tags/v0.45.0";
      type = "git";
      submodules = true;
    };

    hyprland-contrib.url = "github:hyprwm/contrib";
    hyprland-contrib.inputs.nixpkgs.follows = "nixpkgs";

    # Software inputs
    user-js.url = "github:arkenfox/user.js";
    user-js.flake = false;

    walker.url = "github:abenz1267/walker";
    walker.inputs.nixpkgs.follows = "nixpkgs-unstable";
    walker.inputs.flake-parts.follows = "flake-parts";

    stylix.url = "github:binarin/stylix";
    stylix.inputs.home-manager.follows = "home-manager";
    stylix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";

    nixos-generators.url = "github:nix-community/nixos-generators/1.8.0";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs-unstable";

    caddy-cloudflare.url = "github:binarin/caddy-with-plugins";
    caddy-cloudflare.inputs.nixpkgs.follows = "nixpkgs"; # golang < 1.23 - see https://github.com/nix-community/gomod2nix/issues/117

    arion.url = "github:hercules-ci/arion";
    arion.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs:
    let
      systems = [ "x86_64-linux" ];
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;
      imports = [
        ./modules/flake-parts/autowire.nix
        ./modules/flake-parts/deploy.nix
        ./modules/flake-parts/devshell.nix
        ./modules/flake-parts/formatter.nix
      ];
    };
}
