{
  self,
  inputs,
  config,
  ...
}:
let
  selfLib = self.lib.self;
in
{
  flake.deploy.nodes.media = {
    hostname = config.inventory.ipAllocation."media".home.primary.address;
    profiles.system = {
      sshUser = "root";
      path = self.lib.deploy-nixos self.nixosConfigurations.media;
    };
  };

  flake.nixosConfigurations.media = inputs.nixpkgs.lib.nixosSystem {
    # system = "x86_64-linux";
    pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
    specialArgs.inventoryHostName = "media";
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
      key = "nixos-config.modules.nixos.media-configuration";
      imports = [
        self.nixosModules.baseline
        self.nixosModules.lxc
        inputs.arion.nixosModules.arion

        self.nixosModules.linkwarden
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
          sopsFile = "${selfLib.file' "secrets/webservers.yaml"}";
          restartUnits = [ "caddy.service" ];
        };

        systemd.services.caddy.serviceConfig.AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_BIND_SERVICE";
        systemd.services.caddy.serviceConfig.LoadCredential =
          "cloudflare-api-token:${config.sops.secrets.cloudflare-api-key.path}";

        services.caddy = {
          enable = true;
          enableReload = false; # fails to reload when new hosts are added
          package = self.packages."${pkgs.stdenv.hostPlatform.system}".caddy-with-cloudflare-dns;
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
                    image = "bbilly1/tubearchivist-es:${tags.archivist-es}";
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

        services.caddy.virtualHosts."ta.binarin.info".extraConfig = ''
          reverse_proxy http://127.0.0.1:8001
          import letsencrypt
        '';

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

        users.users.sabnzbd.extraGroups = [ "usenet" ];

        sops.secrets."sabnzbd/username" = { };
        sops.secrets."sabnzbd/password" = { };
        sops.secrets."sabnzbd/api_key" = { };
        sops.secrets."sabnzbd/nzb_key" = { };
        sops.secrets."sabnzbd/eweka_username" = { };
        sops.secrets."sabnzbd/eweka_password" = { };

        sops.templates."sabnzbd/misc-secrets" = {
          content = ''
            [misc]
            api_key = ${config.sops.placeholder."sabnzbd/api_key"}
            nzb_key = ${config.sops.placeholder."sabnzbd/nzb_key"}
            username = ${config.sops.placeholder."sabnzbd/username"}
            password = ${config.sops.placeholder."sabnzbd/password"}
          '';
          restartUnits = [ "sabnzbd.service" ];
        };

        sops.templates."sabnzbd/server-secrets" = {
          content = ''
            [servers]
            [[news.eweka.nl]]
            username = ${config.sops.placeholder."sabnzbd/eweka_username"}
            password = ${config.sops.placeholder."sabnzbd/eweka_password"}
          '';
          restartUnits = [ "sabnzbd.service" ];
        };

        services.sabnzbd = {
          enable = true;
          configFile = null;
          secretFiles = [
            config.sops.templates."sabnzbd/misc-secrets".path
            config.sops.templates."sabnzbd/server-secrets".path
          ];
          settings = {
            misc = {
              helpful_warnings = true;
              queue_complete = "";
              queue_complete_pers = false;
              bandwidth_perc = 100;
              refresh_rate = 1;
              interface_settings = "";
              queue_limit = 20;
              config_lock = false;
              fixed_ports = true;
              notified_new_skin = false;
              direct_unpack_tested = true;
              sorters_converted = true;
              check_new_rel = true;
              auto_browser = false;
              language = "en";
              enable_https_verification = true;
              host = "127.0.0.1";
              port = 8085;
              https_port = "";
              bandwidth_max = "";
              cache_limit = "1G";
              web_dir = "Glitter";
              web_color = "Auto";
              https_cert = "server.cert";
              https_key = "server.key";
              https_chain = "";
              enable_https = false;
              inet_exposure = 0;
              socks5_proxy_url = "";
              permissions = 775;
              download_dir = "/media/usenet/Downloads/incomplete";
              download_free = "";
              complete_dir = "/media/usenet/Downloads/complete";
              complete_free = "";
              fulldisk_autoresume = false;
              script_dir = "";
              nzb_backup_dir = "";
              admin_dir = "/var/lib/sabnzbd/admin";
              backup_dir = "";
              dirscan_dir = "";
              dirscan_speed = 5;
              password_file = "";
              log_dir = "/var/lib/sabnzbd/logs";
              max_art_tries = 3;
              top_only = false;
              sfv_check = true;
              script_can_fail = false;
              enable_recursive = true;
              flat_unpack = false;
              par_option = "";
              pre_check = false;
              nice = "";
              win_process_prio = 3;
              ionice = "";
              fail_hopeless_jobs = true;
              fast_fail = true;
              auto_disconnect = true;
              no_dupes = false;
              no_series_dupes = false;
              no_smart_dupes = false;
              dupes_propercheck = true;
              pause_on_pwrar = true;
              ignore_samples = false;
              deobfuscate_final_filenames = true;
              auto_sort = "";
              direct_unpack = true;
              propagation_delay = 0;
              folder_rename = true;
              replace_spaces = false;
              replace_underscores = false;
              replace_dots = false;
              safe_postproc = true;
              pause_on_post_processing = false;
              enable_all_par = false;
              sanitize_safe = false;
              cleanup_list = ",";
              unwanted_extensions = ",";
              action_on_unwanted_extensions = 0;
              unwanted_extensions_mode = 0;
              new_nzb_on_failure = false;
              history_retention = "";
              history_retention_option = "all";
              history_retention_number = 1;
              quota_size = "";
              quota_day = "";
              quota_resume = false;
              quota_period = "m";
              enable_tv_sorting = false;
              tv_sort_string = "";
              tv_categories = "tv,";
              enable_movie_sorting = false;
              movie_sort_string = "";
              movie_sort_extra = "-cd%1";
              movie_categories = "movies,";
              enable_date_sorting = false;
              date_sort_string = "";
              date_categories = "tv,";
              schedlines = ",";
              rss_rate = 60;
              ampm = false;
              start_paused = false;
              preserve_paused_state = false;
              enable_par_cleanup = true;
              process_unpacked_par2 = true;
              enable_multipar = true;
              enable_unrar = true;
              enable_7zip = true;
              enable_filejoin = true;
              enable_tsjoin = true;
              overwrite_files = false;
              ignore_unrar_dates = false;
              backup_for_duplicates = false;
              empty_postproc = false;
              wait_for_dfolder = false;
              rss_filenames = false;
              api_logging = true;
              html_login = true;
              disable_archive = false;
              warn_dupl_jobs = false;
              keep_awake = true;
              tray_icon = true;
              allow_incomplete_nzb = false;
              enable_broadcast = true;
              ipv6_hosting = false;
              ipv6_staging = false;
              api_warnings = true;
              no_penalties = false;
              x_frame_options = true;
              allow_old_ssl_tls = false;
              enable_season_sorting = true;
              verify_xff_header = false;
              rss_odd_titles = "nzbindex.nl/, nzbindex.com/, nzbclub.com/";
              quick_check_ext_ignore = "nfo, sfv, srr";
              req_completion_rate = 100.2;
              selftest_host = "self-test.sabnzbd.org";
              movie_rename_limit = "100M";
              episode_rename_limit = "20M";
              size_limit = 0;
              direct_unpack_threads = 3;
              history_limit = 10;
              wait_ext_drive = 5;
              max_foldername_length = 246;
              nomedia_marker = "";
              ipv6_servers = true;
              url_base = "/sabnzbd";
              host_whitelist = "sabnzbd.binarin.info,";
              local_ranges = ",";
              max_url_retries = 10;
              downloader_sleep_time = 10;
              receive_threads = 2;
              switchinterval = 0.005;
              ssdp_broadcast_interval = 15;
              ext_rename_ignore = ",";
              email_server = "";
              email_to = ",";
              email_from = "";
              email_account = "";
              email_pwd = "";
              email_endjob = 0;
              email_full = false;
              email_dir = "";
              email_rss = false;
              email_cats = "*,";
            };
            logging = {
              log_level = 1;
              max_log_size = 5242880;
              log_backups = 5;
            };
            ncenter = {
              ncenter_enable = false;
              ncenter_cats = "*,";
              ncenter_prio_startup = 0;
              ncenter_prio_download = 0;
              ncenter_prio_pause_resume = 0;
              ncenter_prio_pp = 0;
              ncenter_prio_complete = 1;
              ncenter_prio_failed = 1;
              ncenter_prio_disk_full = 1;
              ncenter_prio_new_login = 0;
              ncenter_prio_warning = 0;
              ncenter_prio_error = 0;
              ncenter_prio_queue_done = 0;
              ncenter_prio_other = 1;
            };
            acenter = {
              acenter_enable = false;
              acenter_cats = "*,";
              acenter_prio_startup = 0;
              acenter_prio_download = 0;
              acenter_prio_pause_resume = 0;
              acenter_prio_pp = 0;
              acenter_prio_complete = 1;
              acenter_prio_failed = 1;
              acenter_prio_disk_full = 1;
              acenter_prio_new_login = 0;
              acenter_prio_warning = 0;
              acenter_prio_error = 0;
              acenter_prio_queue_done = 0;
              acenter_prio_other = 1;
            };
            ntfosd = {
              ntfosd_enable = true;
              ntfosd_cats = "*,";
              ntfosd_prio_startup = 0;
              ntfosd_prio_download = 0;
              ntfosd_prio_pause_resume = 0;
              ntfosd_prio_pp = 0;
              ntfosd_prio_complete = 1;
              ntfosd_prio_failed = 1;
              ntfosd_prio_disk_full = 1;
              ntfosd_prio_new_login = 0;
              ntfosd_prio_warning = 0;
              ntfosd_prio_error = 0;
              ntfosd_prio_queue_done = 0;
              ntfosd_prio_other = 1;
            };
            prowl = {
              prowl_enable = false;
              prowl_cats = "*,";
              prowl_apikey = "";
              prowl_prio_startup = -3;
              prowl_prio_download = -3;
              prowl_prio_pause_resume = -3;
              prowl_prio_pp = -3;
              prowl_prio_complete = 0;
              prowl_prio_failed = 1;
              prowl_prio_disk_full = 1;
              prowl_prio_new_login = -3;
              prowl_prio_warning = -3;
              prowl_prio_error = -3;
              prowl_prio_queue_done = -3;
              prowl_prio_other = 0;
            };
            pushover = {
              pushover_token = "";
              pushover_userkey = "";
              pushover_device = "";
              pushover_emergency_expire = 3600;
              pushover_emergency_retry = 60;
              pushover_enable = false;
              pushover_cats = "*,";
              pushover_prio_startup = -3;
              pushover_prio_download = -2;
              pushover_prio_pause_resume = -2;
              pushover_prio_pp = -3;
              pushover_prio_complete = -1;
              pushover_prio_failed = -1;
              pushover_prio_disk_full = 1;
              pushover_prio_new_login = -3;
              pushover_prio_warning = 1;
              pushover_prio_error = 1;
              pushover_prio_queue_done = -3;
              pushover_prio_other = -1;
            };
            pushbullet = {
              pushbullet_enable = false;
              pushbullet_cats = "*,";
              pushbullet_apikey = "";
              pushbullet_device = "";
              pushbullet_prio_startup = 0;
              pushbullet_prio_download = 0;
              pushbullet_prio_pause_resume = 0;
              pushbullet_prio_pp = 0;
              pushbullet_prio_complete = 1;
              pushbullet_prio_failed = 1;
              pushbullet_prio_disk_full = 1;
              pushbullet_prio_new_login = 0;
              pushbullet_prio_warning = 0;
              pushbullet_prio_error = 0;
              pushbullet_prio_queue_done = 0;
              pushbullet_prio_other = 1;
            };
            apprise = {
              apprise_enable = false;
              apprise_cats = "*,";
              apprise_urls = "";
              apprise_target_startup = "";
              apprise_target_startup_enable = 0;
              apprise_target_download = "";
              apprise_target_download_enable = 0;
              apprise_target_pause_resume = "";
              apprise_target_pause_resume_enable = 0;
              apprise_target_pp = "";
              apprise_target_pp_enable = 0;
              apprise_target_complete = "";
              apprise_target_complete_enable = 1;
              apprise_target_failed = "";
              apprise_target_failed_enable = 1;
              apprise_target_disk_full = "";
              apprise_target_disk_full_enable = 0;
              apprise_target_new_login = "";
              apprise_target_new_login_enable = 1;
              apprise_target_warning = "";
              apprise_target_warning_enable = 0;
              apprise_target_error = "";
              apprise_target_error_enable = 0;
              apprise_target_queue_done = "";
              apprise_target_queue_done_enable = 0;
              apprise_target_other = "";
              apprise_target_other_enable = 1;
            };
            nscript = {
              nscript_enable = false;
              nscript_cats = "*,";
              nscript_script = "";
              nscript_parameters = "";
              nscript_prio_startup = 0;
              nscript_prio_download = 0;
              nscript_prio_pause_resume = 0;
              nscript_prio_pp = 0;
              nscript_prio_complete = 1;
              nscript_prio_failed = 1;
              nscript_prio_disk_full = 1;
              nscript_prio_new_login = 0;
              nscript_prio_warning = 0;
              nscript_prio_error = 0;
              nscript_prio_queue_done = 0;
              nscript_prio_other = 1;
            };
            servers."news.eweka.nl" = {
              name = "news.eweka.nl";
              displayname = "news.eweka.nl";
              host = "news.eweka.nl";
              port = 563;
              timeout = 60;
              connections = 8;
              ssl = true;
              ssl_verify = 2;
              ssl_ciphers = "";
              enable = true;
              required = false;
              optional = false;
              retention = 0;
              expire_date = "";
              quota = "";
              usage_at_start = 0;
              priority = 0;
              notes = "";
            };
            categories = {
              "*" = {
                name = "*";
                order = 0;
                pp = 3;
                dir = "";
                newzbin = "";
                priority = 0;
              };
              movies = {
                name = "movies";
                order = 1;
                pp = "";
                script = "Default";
                dir = "";
                newzbin = "";
                priority = -100;
              };
              tv = {
                name = "tv";
                order = 2;
                pp = "";
                script = "Default";
                dir = "";
                newzbin = "";
                priority = -100;
              };
              audio = {
                name = "audio";
                order = 3;
                pp = "";
                script = "Default";
                dir = "";
                newzbin = "";
                priority = -100;
              };
              software = {
                name = "software";
                order = 4;
                pp = "";
                script = "Default";
                dir = "";
                newzbin = "";
                priority = -100;
              };
              prowlarr = {
                name = "prowlarr";
                order = 1;
                pp = "";
                script = "Default";
                dir = "";
                newzbin = "";
                priority = -100;
              };
            };
          };
        };

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
          ALLOWED_HOSTS=tandoor.binarin.info

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
                  config.sops.templates.tandoor-env.path
                ];
              };
              web_recipes.service = {
                restart = "unless-stopped";
                image = "vabene1111/recipes:${tags.web_recipes}";
                ports = [
                  "8081:80"
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

        services.caddy.virtualHosts."homepage.binarin.info".extraConfig =
          let
            customIconsDir = selfLib.dir "dashboard-icons";
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

        services.immich = {
          enable = true;
          mediaLocation = "/mnt/immich";
        };

        services.tailscale.serve = {
          enable = true;
          services.immich.endpoints."tcp:443" = "https://localhost:${builtins.toString config.services.immich.port}";
          services.commafeed.endpoints."tcp:443" = "https://localhost:${builtins.toString config.services.commafeed.environment.QUARKUS_HTTP_PORT}";
        };

        nixos-config.export-metrics.enable = true;

        services.commafeed = {
          enable = true;
          environment = {
            QUARKUS_HTTP_PORT = 9090;
          };
        };
      };
    };
}
