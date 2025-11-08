{ self, ... }:
{
  flake-file.inputs.nix-ai-tools.url = "github:numtide/nix-ai-tools";
  flake-file.inputs.nix-ai-tools.inputs.nixpkgs.follows = "nixpkgs";

  flake.homeModules.claude-code =
    { pkgs, ... }:
    {
      key = "nixos-config.programs.claude-code";

      imports = [
        self.homeModules.impermanence
      ];

      home.packages = [
        self.inputs.nix-ai-tools.packages.${pkgs.system}.claude-code
      ];

      impermanence.persist-files = [
        ".claude.json"
      ];

      impermanence.persist-directories = [
        ".claude"
      ];
    };
}
