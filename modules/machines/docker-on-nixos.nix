{
  self,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.docker-on-nixos = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inherit self inputs;
      hostConfig = {
        isLinux = true;
      };
    };
    modules = [
      self.nixosModules.docker-on-nixos-configuration
    ]
    ++ self.nixosSharedModules;
  };

  flake.nixosModules.docker-on-nixos-configuration =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      create = lxcId: ''
        pct create ${lxcId} /var/lib/vz/template/cache/proxmox-lxc-docker-on-nixos.tar.xz \
          --hostname docker-on-nixos \
          --onboot 1 \
          --cores 4 --features nesting=1 --memory 8192 --swap 0 \
          --unprivileged 1 --ostype unmanaged --arch amd64 \
          --rootfs local-zfs:8 \
          --mp0 local-zfs:1,mp=/sbin,backup=1 \
          --mp1 local-zfs:8,mp=/nix,backup=1 \
          --mp2 local-zfs:8,mp=/persist,backup=1 \
          --mp3 local-zfs:1,mp=/local,backup=0 \
          --net0 name=eth0,bridge=vmbr0,firewall=1,ip=dhcp,type=veth

        cat <<EOF >> /etc/pve/lxc/${lxcId}.conf
        lxc.cgroup2.devices.allow: c 10:200 rwm
        lxc.mount.entry: /dev/net/tun dev/net/tun none bind,create=file
        EOF

        ROOTFS=rpool/data/subvol-${lxcId}-disk-0
        zfs destroy $ROOTFS
        zfs create $ROOTFS
        chown -v 100000:100000 /$ROOTFS
        zfs snapshot rpool/data/subvol-${lxcId}-disk-0@blank

        touch /$ROOTFS/nix-path-registration
        chown -Rv 100000:100000 /$ROOTFS

        cat <<"EOF" | tee /var/lib/vz/snippets/pve-impermanence-hook.pl > /dev/null
        ${config.lib.self.read "pve-impermanence-hook.pl"}
        EOF
        chmod +x /var/lib/vz/snippets/pve-impermanence-hook.pl
        pct set 116 --hookscript local:snippets/pve-impermanence-hook.pl

        pct start ${lxcId}
      '';
    in
    {
      key = "nixos-config.docker-on-nixos-configuration";
      imports = [
        self.nixosModules.default
        "${inputs.nixpkgs}/nixos/modules/profiles/minimal.nix"
        inputs.arion.nixosModules.arion
      ];

      config = {
        networking.hostName = "docker-on-nixos";

        impermanence.enable = true;

        hostConfig.features = [
          "lxc"
          "tailscale"
        ];

        system.stateVersion = "24.11";

        lib.lxc.createCommand = create;

        virtualisation.docker.enable = true;
        virtualisation.docker.autoPrune.enable = true;
        virtualisation.arion.backend = "docker";

        environment.persistence."/persist".directories = [
          "/var/lib/docker"
        ];

        services.caddy.expose-local-http.enable = true;

        # brick-tracker configuration
        services.caddy.expose-local-http.virtualHosts."brick-tracker.binarin.info" = "localhost:3333";

        environment.systemPackages = with pkgs; [ litecli ];

        sops.secrets."bricktracker/rebrickable-api-key" = { };
        sops.templates."bricktracker-env".content = ''
          BK_REBRICKABLE_API_KEY="${config.sops.placeholder."bricktracker/rebrickable-api-key"}"
        '';

        virtualisation.arion.projects.bricktracker = {
          serviceName = "bricktracker-docker-compose";
          settings = {
            services = {
              bricktracker = {
                service = {
                  container_name = "BrickTracker";
                  restart = "unless-stopped";
                  image = "gitea.baerentsen.space/frederikbaerentsen/bricktracker:1.2.1";
                  ports = [ "3333:3333" ];
                  volumes = [
                    "/persist/BrickTracker/data:/data/"
                    "/persist/BrickTracker/instructions:/app/static/instructions/"
                    "/persist/BrickTracker/minifigures:/app/static/minifigures/"
                    "/persist/BrickTracker/parts:/app/static/parts/"
                    "/persist/BrickTracker/sets:/app/static/sets/"
                  ];
                  environment = {
                    BK_DATABASE_PATH = "/data/app.db";
                    BK_MINIFIGURES_FOLDER = "minifigures";
                    BK_RETIRED_SETS_PATH = "/data/retired_sets.csv";
                    BK_THEMES_PATH = "/data/themes.csv";
                  };
                  env_file = [
                    config.sops.templates.bricktracker-env.path
                  ];
                };
              };
            };
          };
        };

        # homebox configuration
        services.caddy.expose-local-http.virtualHosts."homebox.binarin.info" = "http://localhost:7745";
        virtualisation.arion.projects.homebox = {
          serviceName = "home-box-docker-compose";
          settings = {
            services = {
              homebox = {
                service = {
                  image = "ghcr.io/sysadminsmedia/homebox:0.21";
                  container_name = "homebox";
                  restart = "unless-stopped";
                  environment = {
                    HBOX_LOG_LEVEL = "info";
                    HBOX_LOG_FORMAT = "text";
                    HBOX_WEB_MAX_FILE_UPLOAD = "10";
                    HBOX_OPTIONS_ALLOW_REGISTRATION = "false";
                    HBOX_MODE = "production";
                    HBOX_STORAGE_DATA = "/data";
                    HBOX_DATABASE_DRIVER = "sqlite3";
                    HBOX_STORAGE_SQLITE_PATH = "/data/homebox.db?_pragma=busy_timeout=999&_pragma=journal_mode=WAL&_fk=1";
                  };
                  volumes = [
                    "/persist/homebox/data:/data/"
                  ];
                  ports = [
                    "7745:7745"
                  ];
                };
              };
            };
          };
        };
      };
    };
}
