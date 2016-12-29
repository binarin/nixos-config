{config, pkgs, ...}:
{
  require = [];
  environment.systemPackages = with pkgs; [
    bind
    curl
    (emacs25.override {withX = false; withGTK2 = false; withGTK3 = false;})
    gdb
    git
    htop
    iotop
    lsof
    mc
    mosh
    netcat
    nethogs
    nix-repl
    nmap
    openssl
    psmisc
    screen
    strace
    telnet
    tmux
    vim
    wget
    which
    wireshark
    zsh
  ];
}
