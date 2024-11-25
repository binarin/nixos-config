let
  flake = builtins.getFlake "path:${builtins.readFile /etc/nix/inputs/self-absolute-path}";
  host = builtins.head (builtins.split "\n" (builtins.readFile "/etc/hostname"));
  configuration = flake.nixosConfigurations."${host}";
in
configuration.config.nixpkgs.config
