{ self, inputs, ... }:
{
  flake.homeModules.agent-skills =
    { ... }:
    {
      key = "nixos-config.modules.home.agent-skills";

      imports = [ inputs.common.homeModules.agent-skills ];
    };

  flake.homeModules.codex =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.codex";
      imports = [ self.homeModules.agent-skills ];

      programs.codex = {
        enable = true;
        package = pkgs.llm-agents.codex;
      };
    };

  flake.homeModules.claude-code =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.claude-code";

      imports = [ self.homeModules.agent-skills ];

      programs.claude-code = {
        enable = true;
        package = pkgs.llm-agents.claude-code;
      };
    };
}
