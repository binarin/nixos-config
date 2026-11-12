{ self, ... }:
{
  # Skill sources live in files/claude-skills and reach Claude Code through the
  # checkout link declared by claude-skills. pi and codex read their own
  # directories, so they get store links to the same directory instead.
  flake.homeModules.agent-skills =
    { ... }:
    {
      key = "nixos-config.modules.home.agent-skills";

      home.file = {
        ".codex/skills/claudy-comment-detector".source = ../../files/claude-skills/claudy-comment-detector;
        ".pi/agent/skills/claudy-comment-detector".source =
          ../../files/claude-skills/claudy-comment-detector;
      };
    };

  flake.homeModules.claude-skills =
    { ... }:
    {
      key = "nixos-config.modules.home.claude-skills";

      home.localCheckoutLinks.files.".claude/skills" = {
        checkout = "nixos-config";
        source = "files/claude-skills";
        fallback = self.lib.self.dir "claude-skills";
        type = "directory";
      };
    };
}
