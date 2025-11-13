{ self, ... }:
{
  flake.nixosModules.baseline =
    { ... }:
    {
      imports = [
        self.nixosModules.binarin-baseline
        self.nixosModules.eternal-terminal
      ];

      environment.enableAllTerminfo = true;

      i18n.defaultLocale = "nl_NL.UTF-8";
      i18n.extraLocales = [ "all" ];

      programs.bat.enable = true;
      programs.direnv.enable = true;
      programs.fzf = {
        fuzzyCompletion = true;
        keybindings = true;
      };
      programs.git.enable = true;
      programs.htop.enable = true;
      programs.iftop.enable = true;
      programs.iotop.enable = true;
      programs.mosh.enable = true;
      programs.ssh.startAgent = true;
      programs.tcpdump.enable = true;
      programs.tmux.enable = true;
      programs.traceroute.enable = true;
      programs.zoxide.enable = true;
      security.sudo = {
        enable = true;
        wheelNeedsPassword = false;
      };
    };
}
