{ self, ... }:
{
  flake-file.inputs.nix-ai-tools.url = "github:numtide/llm-agents.nix";
  flake-file.inputs.nix-ai-tools.inputs.nixpkgs.follows = "nixpkgs";

  flake.homeModules.ai-tools =
    { pkgs, config, ... }:
    {
      key = "nixos-config.modules.home.ai-tools";
      imports = [
        self.homeModules.impermanence
      ];

      home.packages =
        (with self.inputs.nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system}; [
          claude-code
          gemini-cli
          opencode
        ])
        ++ (with pkgs; [
          llm
          python3Packages.markitdown
        ]);

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
