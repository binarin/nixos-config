{ inputs, lib, ... }:
{
  flake-file.inputs = {
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { ... }:
    {
      treefmt = {
        projectRootFile = "flake.nix";
        programs.nixf-diagnose.enable = lib.mkForce false;
        settings.global.excludes = [
          ".gitattributes"
          "*.org"
          "*.el"
          "*.cfg"
          "*.gpg"
          "*.kdl"
          "*.sh"
          ".terraform"
          ".direnv"
          "secrets/**"
          "terraform/**"
          "ansible/**"
          ".forgejo/**"
          "files/*"
          "justfile"
          ".sops.yaml"
        ];
      };
    };
}
