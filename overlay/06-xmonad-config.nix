self: super: {
  inherit (import ../xmonad-config/default.nix { pkgs = super; }) my-xmonad-config my-xmonad-executable;
}
