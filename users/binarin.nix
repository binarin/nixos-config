{pkgs, ...}:
{
  imports = [
  ];
  users.extraUsers = {
    binarin = {
      description = "Alexey Lebedeff";
      uid = 1000;
      isNormalUser = true;
      shell = "/run/current-system/sw/bin/zsh";
      extraGroups = [ "networkmanager" "docker" "libvirtd" "wheel" "dialout" "vboxusers" "wireshark" ];
    };
  };
  programs.zsh.enable = true;
  programs.zsh.zsh-autoenv = {
    enable = true;
    package = pkgs.zsh-autoenv;
  };
  programs.zsh.ohMyZsh = {
    enable = true;
    plugins = [
      "colored-man-pages"
      "dirpersist"
    ];
    theme = "gianu";
  };
  programs.bash.enableCompletion = true;
  programs.zsh.interactiveShellInit = ''
    PATH=$PATH:${pkgs.autojump}/bin
    . ${pkgs.autojump}/share/autojump/autojump.zsh
  '';
}
