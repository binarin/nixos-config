{self, ...}: {
  flake.homeModules.claude-code = { pkgs, lib, config, ... }: {
    key = "nixos-config.programs.claude-code";

    imports = [
      self.homeModules.impermanence
    ];

    home.packages = with pkgs; [
      claude-code
    ];

    impermanence.persist-files = [
      ".claude.json"
    ];

    impermanence.persist-directories = [
        ".claude"
    ];
  };
}
