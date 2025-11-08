# -*- nix -*-
{
  pkgs,
  ...
}:
{
  # tailscale serve --bg --tls-terminated-tcp 1143 1143
  # tailscale serve --bg --tls-terminated-tcp 1025 1025
  # gpg --pinentry-mode loopback --quick-gen-key --passphrase '' 'Mail LXC <mail-lxc@binarin.info>'

  environment.systemPackages = with pkgs; [
    imapsync
    protonmail-bridge
    gnupg
    pinentry-tty
    pass
  ];

  systemd.services.protonmail-bridge = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      User = "protonmail-bridge";
    };
    path = with pkgs; [
      protonmail-bridge
      gnupg
      pass
    ];
    script = ''
      protonmail-bridge -n
    '';
  };

  users.users.protonmail-bridge = {
    isNormalUser = true;
    group = "protonmail-bridge";
  };
  users.groups.protonmail-bridge = {
  };
}
