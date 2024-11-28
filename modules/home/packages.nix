{ pkgs, config, ... }:
{
  home.packages = with pkgs; [
    # Unix tools
    ripgrep # Better `grep`
    fd
    sd
    tree
    gnumake

    # Nix dev
    cachix
    nil # Nix language server
    nix-info
    nixpkgs-fmt

    # On ubuntu, we need this less for `man home-configuration.nix`'s pager to
    # work.
    less

    mc
    lsd
  ];
}
