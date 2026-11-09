{ self, inputs, ... }:
{
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
          pkgs.pi-with-extensions
          codex
        ]
        ++ [
          # pkgs.bleeding.llm-agents.pi
        ]
        ++ (lib.optionals osConfig.services.graphical-desktop.enable [
          # claude-code-router
        ]);

      home.localCheckoutLinks.files = {
        ".claude/skills" = {
          checkout = "nixos-config";
          source = "files/claude-skills";
          fallback = self.lib.self.dir "claude-skills";
          type = "directory";
        };
        ".pi/agent/models.json" = {
          checkout = "nixos-config";
          source = "files/pi-agent/models.json";
          fallback = self.lib.self.file "pi-agent/models.json";
        };
        ".pi/agent/keybindings.json" = {
          checkout = "nixos-config";
          source = "files/pi-agent/keybindings.json";
          fallback = self.lib.self.file "pi-agent/keybindings.json";
        };
      };

      home.sessionVariables = {
        # PI_CODING_AGENT_DIR = "${config.home.homeDirectory}/personal-workspace/my-pi/.pi/agent";
        # ANTHROPIC_API_KEY = "-";
        # ANTHROPIC_BASE_URL = "http://aperture.lynx-lizard.ts.net";
        # ANTHROPIC_DEFAULT_HAIKU_MODEL = "deepseek/deepseek-v4-flash";
        # ANTHROPIC_DEFAULT_SONNET_MODEL = "deepseek/deepseek-v4-pro[1m]";
        # ANTHROPIC_DEFAULT_OPUS_MODEL = "z-ai-coding-plan/glm-5.2[1m]";
        # CLAUDE_CODE_AUTO_COMPACT_WINDOW = "1000000";
        # CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
        # API_TIMEOUT_MS = "3000000";
      };

      impermanence.persist-files = [
        ".claude.json"
      ];

      impermanence.persist-directories = [
        ".claude"
        ".pi"
      ];
    };
}
