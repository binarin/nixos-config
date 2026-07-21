{
  self,
  inputs,
  config,
  lib,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.deploy.nodes.media = {
    hostname = config.inventory.ipAllocation."media".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.media;
    };
  };

  clan.inventory.machines.media = {
    deploy.targetHost = flakeConfig.inventory.ipAllocation.media.home.primary.address;
  };

  clan.inventory.instances.acme.roles.client.machines.media.settings = {
    domain = "media.clan.binarin.info";
    extraDomainNames = [
      "navidrome.binarin.info"
      "jellyfin.binarin.info"
      "ta.binarin.info"
      "qbittorrent.binarin.info"
      "sabnzbd.binarin.info"
      "prowlarr.binarin.info"
      "radarr.binarin.info"
      "grocy.binarin.info"
      "tandoor.binarin.info"
      "homepage.binarin.info"
      "atuin.binarin.info"
      "org.binarin.info"
    ];
    reloadServices = [ "nginx.service" ];
  };

  clan.machines.media = {
    imports = [
      self.nixosModules.media-configuration
    ];
    nixpkgs.pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
  };

  flake.nixosConfigurations.media = lib.mkForce (
    self.clan.nixosConfigurations.media.extendModules {
      specialArgs.inventoryHostName = "media";
    }
  );

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

      # TLS certs come from the clan `acme` module: one per-machine cert covering
      # all of media's public hostnames as SANs, pulled + decrypted to this path.
      sslCert = "/var/lib/ssl-cert/full.pem";
      proxyVhost = upstream: {
        addSSL = true;
        sslCertificate = sslCert;
        sslCertificateKey = sslCert;
        locations."/" = {
          proxyPass = upstream;
          proxyWebsockets = true;
        };
      };

      # Non-secret half of tandoor's env-file (the secret half — SECRET_KEY and
      # POSTGRES_PASSWORD — comes from the `tandoor-env` clan var). Both are passed
      # to the arion containers via a two-element env_file list.
      tandoorEnvConfig = pkgs.writeText "tandoor-env-config" ''
        # allowed hosts (see documentation), should be set to your hostname(s) but might be * (default) for some proxies/providers
        ALLOWED_HOSTS=tandoor.binarin.info

        # add only a database password if you want to run with the default postgres, otherwise change settings accordingly
        DB_ENGINE=django.db.backends.postgresql
        POSTGRES_HOST=db_recipes
        POSTGRES_DB=djangodb
        POSTGRES_PORT=5432
        POSTGRES_USER=djangouser
      '';
    in
    {
      key = "nixos-config.modules.nixos.media-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        inputs.arion.nixosModules.arion
      ];

      config = {
        networking.hostName = "media";

        home-manager.users.binarin.home.packages = with pkgs; [
          beets
          shntool
          cuetools
          flac
          monkeysAudio
          python3Packages.chardet
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

        clan.core.vars.generators.samba-passwords = {
          prompts.binarin.description = "Samba password for user binarin";
          files.binarin = {
            secret = true;
            restartUnits = [ "update-samba-passwords.service" ];
          };
          script = ''
            cat $prompts/binarin > $out/binarin
          '';
        };

        # Random samba password for the dedicated `hass` user
        # (HomeAssistantBackup share). Auto-generated, no prompt.
        clan.core.vars.generators.hass-backup-password = {
          files.password = {
            secret = true;
            restartUnits = [ "update-samba-passwords.service" ];
          };
          runtimeInputs = [ pkgs.openssl ];
          script = ''
            openssl rand -base64 32 | tr -d '\n' > $out/password
          '';
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

        # Dedicated samba-only user for the HomeAssistantBackup share.
        users.groups.hass = { };
        users.users.hass = {
          isSystemUser = true;
          group = "hass";
          home = "/var/empty";
          createHome = false;
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
            double ${config.clan.core.vars.generators.samba-passwords.files.binarin.path} \
              | ${pkgs.samba}/bin/smbpasswd -L -a -s binarin
            double ${config.clan.core.vars.generators.hass-backup-password.files.password.path} \
              | ${pkgs.samba}/bin/smbpasswd -L -a -s hass
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
          "Z- /mnt/hass-backup 02775 hass hass -"
          "d /var/lib/org.binarin.info 02750 binarin nginx -"
        ];

        nix.gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 30d";
        };

        # Public *.binarin.info sites are served by nginx off the clan `acme` cert.
        # These names resolve to media's tailscale node IP, so nginx must listen on
        # all interfaces (nginx default: 0.0.0.0 + [::]), exactly as the old caddy
        # did. `tailscale.serve` here uses *named services* on their own service IPs
        # (immich/commafeed), which don't occupy the node IP:443 — so no conflict.
        services.nginx = {
          enable = true;
          recommendedProxySettings = true;
          virtualHosts = {
            "navidrome.binarin.info" = proxyVhost "http://127.0.0.1:4533";
            "jellyfin.binarin.info" = proxyVhost "http://127.0.0.1:8096";
            "ta.binarin.info" = proxyVhost "http://127.0.0.1:8001";
            "qbittorrent.binarin.info" = proxyVhost "http://127.0.0.1:8080";
            "sabnzbd.binarin.info" = proxyVhost "http://127.0.0.1:8085";
            "prowlarr.binarin.info" = proxyVhost "http://127.0.0.1:9696";
            "radarr.binarin.info" = proxyVhost "http://127.0.0.1:7878";
            "atuin.binarin.info" = proxyVhost "http://127.0.0.1:8888";

            # grocy already owns the nginx vhost named "grocy.binarin.info" (its
            # internal http listener on 127.0.0.1:64084), so the public TLS vhost
            # uses a distinct attr key with an explicit serverName.
            "grocy-public" = (proxyVhost "http://127.0.0.1:64084") // {
              serverName = "grocy.binarin.info";
            };

            "tandoor.binarin.info" = {
              addSSL = true;
              sslCertificate = sslCert;
              sslCertificateKey = sslCert;
              locations."/media/" = {
                alias = "/var/lib/tandoor/mediafiles/";
                extraConfig = ''
                  add_header Content-Disposition "attachment";
                '';
              };
              locations."/static/".alias = "/var/lib/tandoor/staticfiles/";
              locations."/" = {
                proxyPass = "http://127.0.0.1:8081";
                proxyWebsockets = true;
              };
            };

            "homepage.binarin.info" = {
              addSSL = true;
              sslCertificate = sslCert;
              sslCertificateKey = sslCert;
              locations."/custom-icons/" = {
                alias = "${selfLib.dir "dashboard-icons"}/";
                extraConfig = ''
                  autoindex on;
                '';
              };
              locations."/" = {
                proxyPass = "http://127.0.0.1:8082";
                proxyWebsockets = true;
              };
            };

            "org.binarin.info" = {
              addSSL = true;
              sslCertificate = sslCert;
              sslCertificateKey = sslCert;
              root = "/var/lib/org.binarin.info";
              locations."/".extraConfig = ''
                autoindex on;
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

        virtualisation.arion.backend = "docker";

        # tubearchivist: raw secrets imported 1:1, then composed into the two
        # env-file fragments the arion containers consume via env_file.
        clan.core.vars.generators.tubearchivist-secrets = {
          prompts.elastic-password.description = "TubeArchivist ELASTIC_PASSWORD";
          prompts.initial-username.description = "TubeArchivist TA_USERNAME";
          prompts.initial-password.description = "TubeArchivist TA_PASSWORD";
          files.elastic-password.secret = true;
          files.initial-username.secret = true;
          files.initial-password.secret = true;
          script = ''
            cat $prompts/elastic-password > $out/elastic-password
            cat $prompts/initial-username > $out/initial-username
            cat $prompts/initial-password > $out/initial-password
          '';
        };
        clan.core.vars.generators.tubearchivist-env = {
          dependencies = [ "tubearchivist-secrets" ];
          files.elastic-env.secret = true;
          files.ta-env.secret = true;
          script = ''
            printf 'ELASTIC_PASSWORD="%s"\n' "$(cat $in/tubearchivist-secrets/elastic-password)" > $out/elastic-env
            printf 'TA_USERNAME="%s"\nTA_PASSWORD="%s"\n' \
              "$(cat $in/tubearchivist-secrets/initial-username)" \
              "$(cat $in/tubearchivist-secrets/initial-password)" > $out/ta-env
          '';
        };

        virtualisation.arion.projects.tubearchivist = {
          serviceName = "tubearchivist-docker-compose";
          settings = {
            services =
              let
                tags = builtins.fromJSON (builtins.readFile ./tubearchivist.json);
              in
              {
                tubearchivist = {
                  service = {
                    container_name = "tubearchivist";
                    restart = "unless-stopped";
                    image = "bbilly1/tubearchivist:${tags.tubearchivist}";
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
                      config.clan.core.vars.generators.tubearchivist-env.files.elastic-env.path
                      config.clan.core.vars.generators.tubearchivist-env.files.ta-env.path
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
                    image = "bbilly1/tubearchivist-es:${tags.archivist-es}";
                    container_name = "archivist-es";
                    restart = "unless-stopped";
                    env_file = [ config.clan.core.vars.generators.tubearchivist-env.files.elastic-env.path ];
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
                    image = "redis:${tags.archivist-redis}";
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

        virtualisation.arion.projects.qbittorrent = {
          serviceName = "qbittorrent-docker-compose";
          settings.services =
            let
              tags = builtins.fromJSON (builtins.readFile ./qbittorrent.json);
            in
            {
              qbittorrent = {
                service = {
                  image = "lscr.io/linuxserver/qbittorrent:${tags.qbittorrent}";
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
        };

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

        users.users.sabnzbd.extraGroups = [ "usenet" ];

        # sabnzbd: raw secrets imported 1:1, then composed into the two ini
        # fragments sabnzbd reads via secretFiles (owned by the sabnzbd user).
        clan.core.vars.generators.sabnzbd-secrets = {
          prompts.api_key.description = "sabnzbd api_key";
          prompts.nzb_key.description = "sabnzbd nzb_key";
          prompts.username.description = "sabnzbd username";
          prompts.password.description = "sabnzbd password";
          prompts.eweka_username.description = "sabnzbd news.eweka.nl username";
          prompts.eweka_password.description = "sabnzbd news.eweka.nl password";
          files.api_key.secret = true;
          files.nzb_key.secret = true;
          files.username.secret = true;
          files.password.secret = true;
          files.eweka_username.secret = true;
          files.eweka_password.secret = true;
          script = ''
            for f in api_key nzb_key username password eweka_username eweka_password; do
              cat "$prompts/$f" > "$out/$f"
            done
          '';
        };
        clan.core.vars.generators.sabnzbd-conf = {
          dependencies = [ "sabnzbd-secrets" ];
          files.misc-secrets = {
            secret = true;
            owner = "sabnzbd";
            restartUnits = [ "sabnzbd.service" ];
          };
          files.server-secrets = {
            secret = true;
            owner = "sabnzbd";
            restartUnits = [ "sabnzbd.service" ];
          };
          script = ''
            {
              printf '[misc]\n'
              printf 'api_key = %s\n' "$(cat $in/sabnzbd-secrets/api_key)"
              printf 'nzb_key = %s\n' "$(cat $in/sabnzbd-secrets/nzb_key)"
              printf 'username = %s\n' "$(cat $in/sabnzbd-secrets/username)"
              printf 'password = %s\n' "$(cat $in/sabnzbd-secrets/password)"
            } > $out/misc-secrets
            {
              printf '[servers]\n'
              printf '[[news.eweka.nl]]\n'
              printf 'username = %s\n' "$(cat $in/sabnzbd-secrets/eweka_username)"
              printf 'password = %s\n' "$(cat $in/sabnzbd-secrets/eweka_password)"
            } > $out/server-secrets
          '';
        };

        services.sabnzbd = {
          enable = true;
          configFile = null;
          secretFiles = [
            config.clan.core.vars.generators.sabnzbd-conf.files.misc-secrets.path
            config.clan.core.vars.generators.sabnzbd-conf.files.server-secrets.path
          ];
          settings = {
            misc = {
              port = 8085;
              bandwidth_perc = 100;
              cache_limit = "1G";
              permissions = 775;
              download_dir = "/media/usenet/Downloads/incomplete";
              complete_dir = "/media/usenet/Downloads/complete";
              admin_dir = "/var/lib/sabnzbd/admin";
              log_dir = "/var/lib/sabnzbd/logs";
              url_base = "/sabnzbd";
              host_whitelist = "sabnzbd.binarin.info,";
              deobfuscate_final_filenames = true;
              enable_season_sorting = true;
              direct_unpack = true;
              history_retention_option = "all";
              direct_unpack_threads = 3;
              rss_odd_titles = "nzbindex.nl/, nzbindex.com/, nzbclub.com/";
              quick_check_ext_ignore = "nfo, sfv, srr";
              req_completion_rate = "100.2";
            };
            servers."news.eweka.nl" = {
              name = "news.eweka.nl";
              displayname = "news.eweka.nl";
              host = "news.eweka.nl";
              ssl_verify = 2;
            };
            categories = {
              movies = {
                name = "movies";
                order = 1;
                script = "Default";
                priority = -100;
              };
              tv = {
                name = "tv";
                order = 2;
                script = "Default";
                priority = -100;
              };
              audio = {
                name = "audio";
                order = 3;
                script = "Default";
                priority = -100;
              };
              software = {
                name = "software";
                order = 4;
                script = "Default";
                priority = -100;
              };
              prowlarr = {
                name = "prowlarr";
                order = 1;
                script = "Default";
                priority = -100;
              };
            };
          };
        };

        services.prowlarr.enable = true;

        services.radarr.enable = true;
        users.users.radarr.extraGroups = [
          "usenet"
          "jellyfin"
        ];

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

        # tandoor configuration
        # NB: preserving the pre-migration behaviour where SECRET_KEY reuses the
        # postgres password (the old `tandoor/secret-key` sops value was never
        # wired in). One raw secret imported 1:1, composed into the secret env.
        clan.core.vars.generators.tandoor-secrets = {
          prompts.postgres-password.description = "tandoor postgres password (also used as SECRET_KEY)";
          files.postgres-password.secret = true;
          script = ''
            cat $prompts/postgres-password > $out/postgres-password
          '';
        };
        clan.core.vars.generators.tandoor-env = {
          dependencies = [ "tandoor-secrets" ];
          files.secret-env.secret = true;
          script = ''
            pw="$(cat $in/tandoor-secrets/postgres-password)"
            {
              printf 'SECRET_KEY=%s\n' "$pw"
              printf 'POSTGRES_PASSWORD=%s\n' "$pw"
            } > $out/secret-env
          '';
        };

        virtualisation.arion.projects.tandoor = {
          serviceName = "tandoor-docker-compose";
          settings.docker-compose.volumes.nginx_config = { };
          settings.services =
            let
              tags = builtins.fromJSON (builtins.readFile ./tandoor.json);
            in
            {
              db_recipes.service = {
                restart = "unless-stopped";
                image = "postgres:${tags.db_recipes}";
                volumes = [
                  "/var/lib/tandoor/postgresql:/var/lib/postgresql/data"
                ];
                env_file = [
                  "${tandoorEnvConfig}"
                  config.clan.core.vars.generators.tandoor-env.files.secret-env.path
                ];
              };
              web_recipes.service = {
                restart = "unless-stopped";
                image = "vabene1111/recipes:${tags.web_recipes}";
                ports = [
                  "8081:80"
                ];
                env_file = [
                  "${tandoorEnvConfig}"
                  config.clan.core.vars.generators.tandoor-env.files.secret-env.path
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
                  (svc "Unifi" "https://unifi.lynx-lizard.ts.net/" "unifi.svg")
                  (svc "Proxmox Raum" "https://raum.lynx-lizard.ts.net" "proxmox.svg")
                  (svc "Proxmox Bael" "https://bael.lynx-lizard.ts.net" "proxmox.svg")
                  (svc "Proxmox Backup Server - bael" "https://bael.lynx-lizard.ts.net:8007/" "/custom-icons/pbs.png")
                  (svc "Proxmox Backup Server - hetzner" "https://pbs-hetzner.binarin.info:8007/"
                    "/custom-icons/pbs.png"
                  )
                  (svc "tinypilot wired" "https://${config.inventory.ipAllocation.tinypilot.home.primary.address}/"
                    "/custom-icons/tiny-pilot.png"
                  )
                  (svc "qdevice nanokvm" "http://${config.inventory.ipAllocation.qdevice-kvm.home.primary.address}/"
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

        # atuin history DB lives on the standalone `postgres` machine (clan
        # postgresql instance). The password is composed into an ATUIN_DB_URI
        # env-file (tandoor-env pattern) so it never enters /nix/store.
        clan.core.vars.generators.atuin-db-env = {
          dependencies = [ "postgresql-postgres-atuin-atuin" ];
          files.env = {
            secret = true;
            restartUnits = [ "atuin.service" ];
          };
          script = ''
            printf 'ATUIN_DB_URI=postgresql://atuin:%s@postgres.lynx-lizard.ts.net:5432/atuin?sslmode=require\n' \
              "$(cat $in/postgresql-postgres-atuin-atuin/password)" > $out/env
          '';
        };

        services.atuin = {
          enable = true;
          database.createLocally = false;
          database.uri = lib.mkForce null;
          environmentFile = config.clan.core.vars.generators.atuin-db-env.files.env.path;
        };

        services.samba.settings.Workspace = smbShareStandartOptions // {
          path = "/media/workspace";
        };

        # Home Assistant backups: dedicated `hass` user, read-write, restricted
        # to that user only (separate credentials from the binarin shares).
        services.samba.settings.HomeAssistantBackup = {
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "valid users" = "hass";
          "force user" = "hass";
          "force group" = "hass";
          "create mask" = "0664";
          "force create mode" = "0664";
          "directory mask" = "2775";
          "force directory mode" = "2775";
          path = "/mnt/hass-backup";
        };

        system.stateVersion = "24.05";

        networking.firewall.allowedTCPPorts = [
          80
          443
        ];

        services.immich = {
          enable = true;
          mediaLocation = "/mnt/immich";
        };

        services.tailscale.serve.enable = true;
        services.tailscale.serve.services.immich = {
          serviceName = "immich";
          protocol = "https";
          target = "localhost:${builtins.toString config.services.immich.port}";
        };

        nixos-config.export-metrics.enable = true;

        services.tailscale.serve.services.commafeed = {
          serviceName = "commafeed";
          protocol = "https";
          target = "localhost:${builtins.toString config.services.commafeed.environment.QUARKUS_HTTP_PORT}";
        };

        services.commafeed = {
          enable = true;
          environment = {
            QUARKUS_HTTP_PORT = 9090;
          };
        };
      };
    };
}
