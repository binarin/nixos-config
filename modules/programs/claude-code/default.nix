{ self, ... }:
{
  flake-file.inputs.nix-ai-tools.url = "github:numtide/nix-ai-tools";
  flake-file.inputs.nix-ai-tools.inputs.nixpkgs.follows = "nixpkgs";

  flake.homeModules.claude-code =
    { pkgs, lib, config, ... }:
    {
      key = "nixos-config.modules.home.claude-code";
      imports = [
        self.homeModules.impermanence
      ];

      home.packages = [
        self.packages.${pkgs.stdenv.hostPlatform.system}.beads
        self.inputs.nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
        self.inputs.nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system}.claudebox
      ];

      home.file.".claude/skills/".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/claude-skills";

      impermanence.persist-files = [
        ".claude.json"
      ];

      impermanence.persist-directories = [
        ".claude"
      ];
    };
}
