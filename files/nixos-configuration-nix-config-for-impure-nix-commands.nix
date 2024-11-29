let
  flake =
    with builtins;
    getFlake (unsafeDiscardStringContext (readFile /etc/nix/inputs/self-absolute-path));
  host = builtins.head (builtins.split "\n" (builtins.readFile "/etc/hostname"));
  configuration = flake.nixosConfigurations."${host}";
in
configuration.config.nixpkgs.config
