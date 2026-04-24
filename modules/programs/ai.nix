{ self, ... }:
{
  flake-file.inputs.nix-ai-tools.url = "github:numtide/llm-agents.nix";
  flake-file.inputs.nix-ai-tools.inputs.nixpkgs.follows = "nixpkgs";

  flake.homeModules.ai-tools =
    {
      inputs',
      pkgs,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.home.ai-tools";
      imports = [
        self.homeModules.impermanence
        self.homeModules.claude-completion
      ];

      home.packages = (
        with inputs'.nix-ai-tools.packages;
        [
          # claude-code
          # claude-code-acp
          # claude-code-router
          # gemini-cli
          # opencode
          # pi
          # workmux
        ]
      );

      home.file.".claude/skills/".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/claude-skills";

      home.file.".pi/agent/models.json".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/pi-agent/models.json";

      impermanence.persist-files = [
        ".claude.json"
      ];

      impermanence.persist-directories = [
        ".claude"
      ];
    };
}
