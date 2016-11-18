{config, pkgs, ...}:
let
  blog = "binarin.ru";
  script = pkgs.stdenv.mkDerivation {
    name = "blog-builder";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cat <<"EOF" > $out/bin/blog-builder
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      if [[ ! -f /home/jekyll/blog ]]; then
        ${pkgs.git}/bin/git clone https://github.com/binarin/blog /home/jekyll/blog
      fi
      ${pkgs.git}/bin/git -C /home/jekyll/blog fetch origin
      ${pkgs.git}/bin/git -C /home/jekyll/blog reset --hard origin/master
      cd /home/jekyll/blog
      ${pkgs.jekyll}/bin/jekyll build
      chmod o+rx /home/jekyll
      EOF
      chmod +x $out/bin/blog-builder
    '';
  };
in
{
  require = [];
  networking.firewall.allowedTCPPorts = [80 443];
  services.nginx.enable = true;
  services.nginx.recommendedTlsSettings = true;
  services.nginx.virtualHosts."${blog}" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      root = "/home/jekyll/blog/_site/";
    };
  };

  users.extraUsers."jekyll" = {
    description = "Separate user for running jekyll";
    home = "/home/jekyll";
    createHome = true;
  };

  systemd.services."${blog}-jekyll" = {
    description = "Fetches blog from github and rebuilds it using jekyll";
    serviceConfig = {
      Type = "oneshot";
      User = "jekyll";
      ExecStart = "${script}/bin/blog-builder";
    };
  };
  systemd.timers."${blog}-jekyll" = {
    description = "Periodically updates my blog from github";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "1hour";
    };
  };
}
