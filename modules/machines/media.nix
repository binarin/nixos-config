{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.media = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      self.nixosModules.media-configuration
    ];
  };

  flake.nixosModules.media-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
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
      key = "nixos-config.media-configuration";
      imports = [
        self.nixosModules.default
        self.nixosModules.lxc
        self.nixosModules.user-binarin
        inputs.arion.nixosModules.arion
      ];

      config = {
        networking.hostName = "media";

        home-manager.users.binarin.home.packages = with pkgs; [
          beets-unstable
          shntool
          cuetools
          flac
          mac
        ];

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

        systemd.network.networks."40-lxc".networkConfig.MulticastDNS = lib.mkForce false;

        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-media-driver
            intel-ocl
            intel-vaapi-driver
            vpl-gpu-rt
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
          radeontop

          git
          git-annex
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

          settings = {
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
          "Z- /media/usenet 02775 root usenet -"
          "d /var/lib/org.binarin.info 02750 binarin caddy -"
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
        systemd.services.caddy.serviceConfig.LoadCredential =
          "cloudflare-api-token:${config.sops.secrets.cloudflare-api-key.path}";

        services.caddy = {
          enable = true;
          enableReload = false; # fails to reload when new hosts are added
          package = pkgs.caddy.withPlugins {
            plugins = [
              "github.com/caddy-dns/cloudflare@v0.0.0-20251022184029-2fc25ee62f40"
            ];
            hash = "sha256-8eyfR+0TY5GEyFmYaW/NXEK2n8iVQYATP9Ma2UzhQCQ";
          };
          extraConfig = ''
            (letsencrypt) {
              tls {
                  dns cloudflare {file.{$CREDENTIALS_DIRECTORY}/cloudflare-api-token}
                  propagation_delay 300s
                  propagation_timeout 1800s
                  dns_ttl 60s
                  resolvers sri.ns.cloudflare.com vera.ns.cloudflare.com
              }
            }
          '';

          virtualHosts = {
            "navidrome.binarin.info" = {
              extraConfig = ''
                reverse_proxy http://127.0.0.1:4533
                import letsencrypt
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
              ffmpeg_7-full = pkgs.ffmpeg_7-full.override {
                withMfx = false;
                withVpl = true;
              };
            };
          };
        };

        services.samba.settings.Movies = smbShareStandartOptions // {
          path = "/media/movies";
          "force user" = "jellyfin";
          "force group" = "jellyfin";
        };

        services.caddy.virtualHosts."jellyfin.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:8096
          import letsencrypt
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
                  image = "bbilly1/tubearchivist:v0.5.8";
                  ports = [ "8001:8000" ];
                  volumes = [
                    "/media/tubearchivist:/youtube"
                    "/var/lib/tubearchivist/cache:/cache"
                  ];
                  environment = {
                    ES_URL = "http://archivist-es:9200"; # needs protocol e.g. http and port
                    REDIS_CON = "redis://archivist-redis:6379";
                    HOST_UID = "1000";
                    HOST_GID = "1000";
                    TA_HOST = "https://ta.binarin.info";
                    TA_AUTO_UPDATE_YTDLP = "release";
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
                  image = "redis";
                  container_name = "archivist-redis";
                  restart = "unless-stopped";
                  environment = {
                    # REDISEARCH_ARGS = "MAXSEARCHRESULTS 30000";
                  };
                  expose = [ "6379" ];
                  volumes = [ "/var/lib/tubearchivist/redis:/data" ];
                  depends_on = [ "archivist-es" ];
                };
              };
            };
          };
        };

        systemd.timers."restart-tubearchivist" = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "*-*-* 03:15:00";
            Unit = "restart-tubearchivist.service";
          };
        };

        systemd.services."restart-tubearchivist" = {
          script = ''
            ${lib.getExe pkgs.docker} restart tubearchivist
          '';
          serviceConfig = {
            Type = "oneshot";
            User = "root";
          };
        };

        services.caddy.virtualHosts."ta.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:8001
          import letsencrypt
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
          import letsencrypt
        '';

        services.samba.settings.Torrents = smbShareStandartOptions // {
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
            ''command="GIT_ANNEX_SHELL_DIRECTORY=/media/annex/annex.git GIT_ANNEX_SHELL_APPENDONLY=true ${lib.getExe' pkgs.git-annex "git-annex-shell"} -c \"$SSH_ORIGINAL_COMMAND\"",restrict ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB binarin@nixos''
          ];
          openssh.authorizedPrincipals = [
            ''command="GIT_ANNEX_SHELL_DIRECTORY=/media/annex/annex.git GIT_ANNEX_SHELL_APPENDONLY=true ${lib.getExe' pkgs.git-annex "git-annex-shell"} -c \"$SSH_ORIGINAL_COMMAND\"",restrict binarin''
          ];
        };

        users.groups.usenet = { };

        services.sabnzbd.enable = true;
        users.users.sabnzbd.extraGroups = [ "usenet" ];

        sops.secrets."sabnzbd/username" = { };
        sops.secrets."sabnzbd/password" = { };
        sops.secrets."sabnzbd/api_key" = { };
        sops.secrets."sabnzbd/nzb_key" = { };
        sops.secrets."sabnzbd/eweka_username" = { };
        sops.secrets."sabnzbd/eweka_password" = { };

        sops.templates."sabnzbd.ini" = {
          content = builtins.readFile (
            pkgs.writeText "sabnzbd.ini" (
              import "${self}/templates/sabnzbd.ini.nix" { ph = config.sops.placeholder; }
            )
          );
          owner = "sabnzbd";
          restartUnits = [ "sabnzbd.service" ];
        };

        services.sabnzbd.configFile = "/var/lib/sabnzbd/sabnzbd.ini";
        systemd.services.sabnzbd.serviceConfig.ExecStartPre = [
          ''
            ${lib.getExe' pkgs.coreutils "cp"} -f ${
              config.sops.templates."sabnzbd.ini".path
            } ${config.services.sabnzbd.configFile}-orig
          ''
          ''
            ${lib.getExe' pkgs.coreutils "cp"} -f ${
              config.sops.templates."sabnzbd.ini".path
            } ${config.services.sabnzbd.configFile}
          ''
        ];

        services.caddy.virtualHosts."sabnzbd.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:8085
          import letsencrypt
        '';

        services.prowlarr.enable = true;
        services.caddy.virtualHosts."prowlarr.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:9696
          import letsencrypt
        '';

        services.radarr.enable = true;
        users.users.radarr.extraGroups = [
          "usenet"
          "jellyfin"
        ];
        services.caddy.virtualHosts."radarr.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:7878
          import letsencrypt
        '';

        # grocy configuration
        services.grocy = {
          enable = true;
          hostName = "grocy.binarin.info";
          nginx.enableSSL = false;
          settings = {
            currency = "EUR";
            culture = "nl";
            calendar.firstDayOfWeek = 1;
          };
        };

        services.nginx.virtualHosts."grocy.binarin.info".listen = [
          {
            addr = "127.0.0.1";
            port = 64084;
          }
        ];

        services.caddy.virtualHosts."grocy.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:64084
          import letsencrypt
        '';

        # tandoor configuration
        sops.secrets."tandoor/secret-key" = { };
        sops.secrets."tandoor/postgres-password" = { };

        sops.templates.tandoor-env.content = ''
          SECRET_KEY=${config.sops.placeholder."tandoor/postgres-password"}

          # allowed hosts (see documentation), should be set to your hostname(s) but might be * (default) for some proxies/providers
          # ALLOWED_HOSTS=recipes.mydomain.com

          # add only a database password if you want to run with the default postgres, otherwise change settings accordingly
          DB_ENGINE=django.db.backends.postgresql
          POSTGRES_HOST=db_recipes
          POSTGRES_DB=djangodb
          POSTGRES_PORT=5432
          POSTGRES_USER=djangouser
          POSTGRES_PASSWORD=${config.sops.placeholder."tandoor/postgres-password"}
        '';

        services.caddy.virtualHosts."tandoor.binarin.info".extraConfig = ''
          handle_path /media/* {
            root * /var/lib/tandoor/mediafiles
            header Content-Disposition `"attachment; filename="{file}"`
            file_server
          }

          handle_path /static/* {
            root * /var/lib/tandoor/staticfiles
            file_server
          }

          reverse_proxy http://127.0.0.1:8081
          import letsencrypt
        '';

        systemd.services.caddy.serviceConfig.ReadOnlyPaths = [
          "/var/lib/tandoor/staticfiles"
          "/var/lib/tandoor/mediafiles"
        ];

        virtualisation.arion.projects.tandoor = {
          serviceName = "tandoor-docker-compose";
          settings.docker-compose.volumes.nginx_config = { };
          settings.services = {
            db_recipes.service = {
              restart = "unless-stopped";
              image = "postgres:16-alpine";
              volumes = [
                "/var/lib/tandoor/postgresql:/var/lib/postgresql/data"
              ];
              env_file = [
                config.sops.templates.tandoor-env.path
              ];
            };
            web_recipes.service = {
              restart = "unless-stopped";
              image = "vabene1111/recipes:1.5.34";
              ports = [
                "8081:8080"
              ];
              env_file = [
                config.sops.templates.tandoor-env.path
              ];
              volumes = [
                "/var/lib/tandoor/staticfiles:/opt/recipes/staticfiles"
                "/var/lib/tandoor/mediafiles:/opt/recipes/mediafiles"
              ];
              depends_on = [ "db_recipes" ];
            };
          };
        };

        # homepage-dashboard configuration
        services.homepage-dashboard =
          let
            svc = title: href: icon: {
              "${title}" = {
                inherit href icon;
              };
            };
          in
          {
            enable = true;
            listenPort = 8082;
            allowedHosts = "homepage.binarin.info,localhost:8082,127.0.0.1:8082";
            services = [
              {
                "Services" = [
                  (svc "Jellyfin" "https://jellyfin.binarin.info/" "jellyfin.svg")
                  (svc "paperless-ngx" "https://paperless.lynx-lizard.ts.net/" "paperless-ngx.svg")
                  (svc "tube-archivist" "https://ta.binarin.info/" "/custom-icons/tube-archivist-logo-dark.png")
                  (svc "Home Assistant" "https://hass.lynx-lizard.ts.net/" "home-assistant.svg")
                  (svc "qbittorrent" "https://qbittorrent.binarin.info/" "qbittorrent.svg")
                  (svc "NextCloud" "https://nc.binarin.info/" "nextcloud.svg")
                ];
              }
              {
                "Servers" = [
                  (svc "Unifi" "https://unifi.binarin.info/" "unifi.svg")
                  (svc "Proxmox Raum" "https://raum.lynx-lizard.ts.net" "proxmox.svg")
                  (svc "Proxmox Bael" "https://bael.lynx-lizard.ts.net" "proxmox.svg")
                  (svc "Proxmox Backup Server - bael" "https://bael.lynx-lizard.ts.net:8007/" "/custom-icons/pbs.png")
                  (svc "Proxmox Backup Server - hetzner" "https://pbs-hetzner.binarin.info:8007/"
                    "/custom-icons/pbs.png"
                  )
                  (svc "tinypilot wired" "https://${config.inventory.ipAllocation.tinypilot.home.primary.address}/"
                    "/custom-icons/tiny-pilot.png"
                  )
                  (svc "nanokvm - home" "http://${config.inventory.ipAllocation.nanokvm.home.primary.address}/"
                    "/custom-icons/sipeed.png"
                  )
                  (svc "nanokvm - ts" "http://nanokvm.lynx-lizard.ts.net/" "/custom-icons/sipeed.png")
                  (svc "NextCloud AIO" "https://nextcloud.lynx-lizard.ts.net:8080/" "nextcloud.svg")
                ];
              }
            ];
            settings = {
              target = "_self";
            };
          };

        services.caddy.virtualHosts."homepage.binarin.info".extraConfig =
          let
            customIconsDir = config.lib.self.base64Dir "dashboard-icons";
          in
          ''
              handle_path /custom-icons/* {
                root * ${customIconsDir}
                @png {
                  path *.png
                }
                header @png Content-Type "image/png"
                file_server browse
              }

              reverse_proxy http://127.0.0.1:8082
            import letsencrypt
          '';

        services.caddy.virtualHosts."atuin.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:8888
          import letsencrypt
        '';

        services.caddy.virtualHosts."unifi.binarin.info".extraConfig = ''
          reverse_proxy ${config.inventory.ipAllocation."usg".home.primary.address}:443 {
            transport http {
              tls
              tls_insecure_skip_verify
            }
          }
          import letsencrypt
        '';

        services.caddy.virtualHosts."org.binarin.info".extraConfig = ''
          root * /var/lib/org.binarin.info
          file_server browse
          import letsencrypt
        '';

        services.atuin.enable = true;

        services.samba.settings.Workspace = smbShareStandartOptions // {
          path = "/media/workspace";
        };

        system.stateVersion = "24.05";

        networking.firewall.allowedTCPPorts = [
          443
        ];

      };
    };
}
