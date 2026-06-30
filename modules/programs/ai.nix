{ self, inputs, ... }:
{
  flake-file.inputs.nix-ai-tools.url = "github:numtide/llm-agents.nix";
  flake-file.inputs.nix-ai-tools.inputs = {
    nixpkgs.follows = "nixpkgs";
    flake-parts.follows = "flake-parts";
    # treefmt-nix.follows = "treemft-nix";
  };

  flake.nixosModules.ai-tools =
    { ... }:
    {
      key = "nixos-config.modules.nixos.ai-tools";
      # nixpkgs.overlays = [
      #   inputs.nix-ai-tools.overlays.shared-nixpkgs
      # ];
    };

  flake.homeModules.ai-tools =
    {
      inputs',
      pkgs,
      config,
      lib,
      osConfig,
      ...
    }:
    {
      key = "nixos-config.modules.home.ai-tools";

      imports = [
        self.homeModules.impermanence
        self.homeModules.claude-completion
      ];

      home.packages =
        with pkgs.llm-agents;
        [
          claude-code
          # claude-code-acp
          # gemini-cli
          # opencode
          pi
          workmux
        ]
        ++ [
          # pkgs.bleeding.llm-agents.pi
        ]
        ++ (lib.optionals osConfig.services.graphical-desktop.enable [
          claude-code-router
        ]);

      home.file.".claude/skills/".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/claude-skills";

      home.file.".pi/agent/models.json".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/pi-agent/models.json";

      home.file.".pi/agent/keybindings.json".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/personal-workspace/nixos-config/files/pi-agent/keybindings.json";

      home.sessionVariables.PI_CODING_AGENT_DIR = "${config.home.homeDirectory}/personal-workspace/my-pi/.pi/agent";

      impermanence.persist-files = [
        ".claude.json"
      ];

      impermanence.persist-directories = [
        ".claude"
        ".pi"
      ];
    };
}
