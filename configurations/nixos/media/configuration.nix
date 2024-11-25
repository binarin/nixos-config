# -*- nix -*-
{
  flake,
  hostConfig,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) inputs;
  inherit (inputs) self;
  smbShareStandartOptions = {
    browseable = "yes";
    "read only" = "yes";
    "guest ok" = "no";
    "write list" = "binarin";
    "force user" = "binarin";
    "force group" = "binarin";
    "create mask" = "0664";
    "force create mode" = "0664";
    "directory mask" = "2775";
    "force directory mode" = "2775";
  };
in
{
  services.avahi = {
    enable = true;
    allowInterfaces = [ "eth0" ];
    publish.enable = true;
    publish.addresses = true;
    extraServiceFiles = {
      smb = ''
        <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
        <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
        <service-group>
          <name replace-wildcards="yes">%h</name>
          <service>
            <type>_smb._tcp</type>
            <port>445</port>
          </service>
        </service-group>
      '';
    };
  };

  nixpkgs.overlays = [ flake.inputs.self.overlays.caddy-cloudflare ];

  systemd.network.networks."40-lxc".networkConfig.MulticastDNS = lib.mkForce false;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-ocl
      intel-vaapi-driver
      onevpl-intel-gpu
    ];
  };

  sops.secrets.tailscale-auth = { };
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets.tailscale-auth.path;
  };

  sops.secrets."samba-passwords/binarin" = {
    restartUnits = [ "update-samba-passwords.service" ];
  };

  environment.systemPackages = with pkgs; [
    docker-compose
    libva-utils
  ];

  virtualisation.docker.enable = true;
  virtualisation.docker.autoPrune.enable = true;

  users.mutableUsers = false;
  users.extraGroups.binarin.gid = 1000;

  users.users.binarin = {
    isNormalUser = true;
    home = "/home/binarin";
    group = "binarin";
    uid = 1000;
  };

  systemd.services.update-samba-passwords = {
    script = ''
      set -euxo pipefail
      double() {
        ${pkgs.coreutils}/bin/cat $1
        echo
        ${pkgs.coreutils}/bin/cat $1
        echo
      }
      shopt -s nullglob
      for file in /run/secrets/samba-passwords/*; do
        user=$(basename $file)
        double $file | ${pkgs.samba}/bin/smbpasswd -L -a -s binarin
        rm -f $file
      done
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };

  services.navidrome = {
    enable = true;
    settings = {
      MusicFolder = "/media/music";
      ReverseProxyWhitelist = "127.0.0.1/32";
    };
  };

  systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = [
    "/run/systemd/resolve/stub-resolv.conf"
  ];

  services.samba = {
    enable = true;
    openFirewall = true;

    shares = {
      "Music" = smbShareStandartOptions // {
        path = "/media/music";
      };
    };
  };

  systemd.tmpfiles.rules = [
    "Z- /media/music 02775 1000 1000 -"
    "Z- /media/movies 02775 ${toString config.users.users.jellyfin.uid} ${toString config.users.groups.jellyfin.gid}  -"
    "Z- /media/torrents 02775 ${toString config.users.users.jellyfin.uid} ${toString config.users.groups.jellyfin.gid}  -"
    "Z- /var/lib/tubearchivist/es 0775 1000 0 -"
    "Z- /var/lib/tubearchivist/redis 0775 999 100 -"
    "Z- /media/tubearchivist 2775 1000 1000 -"
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  sops.secrets.cloudflare-api-key = {
    sopsFile = "${config.lib.self.file' "secrets/webservers.yaml"}";
    restartUnits = [ "caddy.service" ];
  };

  systemd.services.caddy.serviceConfig.AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_BIND_SERVICE";
  systemd.services.caddy.serviceConfig.LoadCredential = "cloudflare-api-token:${config.sops.secrets.cloudflare-api-key.path}";
  services.caddy = {
    enable = true;
    enableReload = false; # fails to reload when new hosts are added
    package = pkgs.caddy-cloudflare;
    virtualHosts = {
      "navidrome.binarin.info" = {
        extraConfig = ''
          reverse_proxy http://127.0.0.1:4533
          tls {
              dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
              resolvers 1.1.1.1
          }
        '';
      };
    };
  };

  users.groups.render.gid = 303;
  # given that 'render' gid on host is 104, this should be added to lxc config:
  # lxc.cgroup2.devices.allow: c 226:128 rwm
  # lxc.mount.entry: /dev/dri/renderD128 dev/dri/renderD128 none bind,optional,create=file
  # lxc.idmap: u 0 100000 65536
  # lxc.idmap: g 0 100000 303
  # lxc.idmap: g 303 104 1
  # lxc.idmap: g 304 100304 65232

  users.groups.jellyfin.gid = 992;
  users.users.jellyfin.uid = 993;
  users.users.jellyfin.extraGroups = [ "render" ];

  systemd.services.jellyfin.serviceConfig.UMask = lib.mkForce "0002";
  services.jellyfin = {
    enable = true;
    openFirewall = true;

    # To use QSV
    package = pkgs.jellyfin.override {
      jellyfin-ffmpeg = pkgs.jellyfin-ffmpeg.override {
        ffmpeg_6-full = pkgs.ffmpeg_6-full.override {
          withMfx = false;
          withVpl = true;
        };
      };
    };
  };

  services.samba.shares.Movies = smbShareStandartOptions // {
    path = "/media/movies";
    "force user" = "jellyfin";
    "force group" = "jellyfin";
  };

  services.caddy.virtualHosts."jellyfin.binarin.info".extraConfig = ''
    reverse_proxy http://127.0.0.1:8096
    tls {
        dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
        resolvers 1.1.1.1
    }
  '';

  virtualisation.arion.backend = "docker";

  sops.secrets."tubearchivist/elastic-password" = { };
  sops.secrets."tubearchivist/initial-username" = { };
  sops.secrets."tubearchivist/initial-password" = { };

  sops.templates."tubearchivist-elastic-env".content = ''
    ELASTIC_PASSWORD="${config.sops.placeholder."tubearchivist/elastic-password"}"
  '';
  sops.templates."tubearchivist-env".content = ''
    TA_USERNAME="${config.sops.placeholder."tubearchivist/initial-username"}"
    TA_PASSWORD="${config.sops.placeholder."tubearchivist/initial-password"}"
  '';

  virtualisation.arion.projects.tubearchivist = {
    serviceName = "tubearchivist-docker-compose";
    settings = {
      services = {
        tubearchivist = {
          service = {
            container_name = "tubearchivist";
            restart = "unless-stopped";
            image = "bbilly1/tubearchivist:v0.4.11";
            ports = [ "8001:8000" ];
            volumes = [
              "/media/tubearchivist:/youtube"
              "/var/lib/tubearchivist/cache:/cache"
            ];
            environment = {
              ES_URL = "http://archivist-es:9200"; # needs protocol e.g. http and port
              REDIS_HOST = "archivist-redis"; # don't add protocol
              HOST_UID = "1000";
              HOST_GID = "1000";
              TA_HOST = "ta.binarin.info";
              TZ = "Europe/Amsterdam"; # set your time zone
            };
            env_file = [
              config.sops.templates.tubearchivist-elastic-env.path
              config.sops.templates.tubearchivist-env.path
            ];
            healthcheck = {
              test = [
                "CMD"
                "curl"
                "-f"
                "http://localhost:8000/health"
              ];
              interval = "2m";
              timeout = "10s";
              retries = 3;
              start_period = "30s";
            };
            depends_on = [
              "archivist-es"
              "archivist-redis"
            ];
          };
          out.service.ulimits = {
            nofile = {
              soft = "4096";
              hard = "4096";
            };
          };
        };

        archivist-es = {
          service = {
            image = "bbilly1/tubearchivist-es";
            container_name = "archivist-es";
            restart = "unless-stopped";
            env_file = [ config.sops.templates.tubearchivist-elastic-env.path ];
            environment = {
              ES_JAVA_OPTS = "-Xms1g -Xmx1g";
              "xpack.security.enabled" = "true";
              "discovery.type" = "single-node";
              "path.repo" = "/usr/share/elasticsearch/data/snapshot";
            };
            expose = [ "9200" ];
            volumes = [ "/var/lib/tubearchivist/es:/usr/share/elasticsearch/data" ];
          };
          # XXX reboot bael for new limits, maybe add them to nixos config too
          # out.service.ulimits = {
          #   memlock = {
          #     soft = "-1";
          #     hard = "-1";
          #   };
          # };
        };
        archivist-redis = {
          service = {
            image = "redis/redis-stack-server";
            container_name = "archivist-redis";
            restart = "unless-stopped";
            environment = {
              REDISEARCH_ARGS = "MAXSEARCHRESULTS 30000";
            };
            expose = [ "6379" ];
            volumes = [ "/var/lib/tubearchivist/redis:/data" ];
            depends_on = [ "archivist-es" ];
          };
        };
      };
    };
  };

  services.caddy.virtualHosts."ta.binarin.info".extraConfig = ''
    reverse_proxy http://127.0.0.1:8001
    tls {
        dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
        resolvers 1.1.1.1
    }
  '';

  virtualisation.arion.projects.qbittorrent = {
    serviceName = "qbittorrent-docker-compose";
    settings.services.qbittorrent = {
      service = {
        image = "lscr.io/linuxserver/qbittorrent:latest";
        container_name = "qbittorrent";
        environment = {
          # So I can move files directly to jellyfin folders using qbittorent itself
          # It would be more correct to only use group, but I don't want to mess with samba permissions anymore
          PUID = config.users.users.jellyfin.uid;
          PGID = config.users.groups.jellyfin.gid;
          TZ = "Europe/Amsterdam";
          UMASK = "0002";
          WEBUI_PORT = 8080;
          TORRENTING_PORT = 6881;
        };
        volumes = [
          "/var/lib/qbittorrent/appdata:/config"
          "/media/torrents:/downloads"
          "/media/movies:/media/movies"
        ];
        ports = [
          "8080:8080"
          "6881:6881"
          "6881:6881/udp"
        ];
        restart = "unless-stopped";
      };
      out.service.ulimits = {
        nofile = {
          soft = "4096";
          hard = "4096";
        };
      };
    };
  };

  services.caddy.virtualHosts."qbittorrent.binarin.info".extraConfig = ''
    reverse_proxy http://127.0.0.1:8080
    tls {
        dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
        resolvers 1.1.1.1
    }
  '';

  services.samba.shares.Torrents = smbShareStandartOptions // {
    path = "/media/torrents";
    "force user" = "jellyfin";
    "force group" = "jellyfin";
  };

  users.groups.annex = { };
  users.users.annex = {
    isNormalUser = true;
    group = "annex";
    home = "/media/annex";
    createHome = true;
    openssh.authorizedKeys.keys = [
      ''command="GIT_ANNEX_SHELL_DIRECTORY=/media/annex/annex.git GIT_ANNEX_SHELL_APPENDONLY=true git-annex-shell -c \"$SSH_ORIGINAL_COMMAND\"",restrict ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos''
    ];
  };

  system.stateVersion = "24.05";
}
