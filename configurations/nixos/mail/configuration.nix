# -*- nix -*-
{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
in
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

  sops.secrets."dovecot-passwd.bin" = {
    owner = config.services.dovecot2.user;
    format = "binary";
    sopsFile = "${config.lib.self.file' ''secrets/mail/dovecot-passwd.bin''}";
  };

  users.users.imapsync = {
    isNormalUser = true;
    group = "imapsync";
  };

  users.groups.imapsync = {};

  sops.secrets."imapsync/host1-password" = {
    owner = "imapsync";
  };
  sops.secrets."imapsync/host2-password" = {
    owner = "imapsync";
  };

  systemd.services.imapsync-proton = {
    script = ''
      cd $HOME
      ${lib.getExe pkgs.imapsync} \
        --nolog \
        --nosslcheck \
        --host1 localhost:1143 \
        --user1 binarin@binarin.info \
        --passfile1 ${config.sops.secrets."imapsync/host1-password".path} \
        --host2 localhost:143 \
        --user2 binarin \
        --passfile2 ${config.sops.secrets."imapsync/host2-password".path} \
        --noresyncflags \
        --noexpunge2 \
        --nouidexpunge2 \
        --automap \
        --subscribeall \
        --exclude ^Labels \
        --exclude ^Folders
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "imapsync";
    };
  };

  systemd.timers.imapsync-proton = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "2min";
    };
  };

  users.users.vmail = {
    isNormalUser = true;
    group = "vmail";
  };
  users.groups.vmail = {};


  systemd.tmpfiles.rules = [
    "d- /var/spool/vmail 0770 vmail vmail -"
    "d- /var/spool/dovecot2 0770 ${config.services.dovecot2.user} ${config.services.dovecot2.group} -"
  ];

  services.dovecot2 = {
    enable = true;
    enablePAM = false;
    mailLocation = "maildir:/var/spool/dovecot2/%u";
    extraConfig = ''
      auth_verbose = yes
      auth_mechanisms = plain
      passdb {
        driver = passwd-file
        args = ${config.sops.secrets."dovecot-passwd.bin".path}
      }
      userdb {
        driver = static
        args = uid=vmail gid=vmail maildir:/var/spool/vmail/%u
      }
      service imap-login {
        inet_listener imap {
          address = 127.0.0.1
        }
      }
    '';
  };

  # --passfile1
  # --passfile2
  # --skipcrossduplicates
  # --noresyncflags
  # --noexpunge2
  # --nouidexpunge2
}
