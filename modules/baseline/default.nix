{ self, ... }:
{
  flake.nixosModules.baseline =
    { pkgs, ... }:
    {
      imports = [
        self.nixosModules.inventory
        self.nixosModules.nix
        self.nixosModules.sshd
        self.nixosModules.security

        self.nixosModules.eternal-terminal

        self.nixosModules.binarin-baseline
      ];

      environment.enableAllTerminfo = true;

      i18n.defaultLocale = "nl_NL.UTF-8";
      i18n.extraLocales = [ "all" ];

      environment.systemPackages = with pkgs; [
        psmisc # pstree
      ];

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
