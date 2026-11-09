{
  flake.nixosModules.wireguard-trappist =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.nixos-config.wireguard-trappist;

      # The trappist network topology - private key, endpoint and internal
      # addressing - lives in this machine's sops file
      # (secrets/<hostname>/secrets.yaml), so it never lands in git.
      # `NetworkManager-ensure-profiles` substitutes these into the profile
      # with envsubst, which also rewrites the peer section name.
      secretVars = {
        TRAPPIST_WG_PRIVATE_KEY = "trappist_wg_private_key";
        TRAPPIST_WG_ADDRESS = "trappist_wg_address";
        TRAPPIST_WG_PEER_PUBLIC_KEY = "trappist_wg_peer_public_key";
        TRAPPIST_WG_ENDPOINT = "trappist_wg_endpoint";
        TRAPPIST_WG_ALLOWED_IPS = "trappist_wg_allowed_ips";
      };
    in
    {
      key = "nixos-config.modules.nixos.wireguard-trappist";

      options.nixos-config.wireguard-trappist = {
        enable = lib.mkEnableOption "client link into the trappist WireGuard network";

        interfaceName = lib.mkOption {
          type = lib.types.strMatching "[a-zA-Z0-9_=+.-]{1,15}";
          default = "trappist";
          description = "Name of the WireGuard interface and of the NetworkManager profile.";
        };

        mtu = lib.mkOption {
          type = lib.types.int;
          default = 1340;
          description = "MTU of the WireGuard interface.";
        };

        persistentKeepalive = lib.mkOption {
          type = lib.types.int;
          default = 25;
          description = "Interval (in seconds) between keepalive packets.";
        };

        dnsListenAddress = lib.mkOption {
          type = lib.types.str;
          default = "127.0.0.153";
          description = ''
            Loopback address the trappist-only dnsmasq listens on (port 53).
            systemd-resolved delegates the trappist zone here.
          '';
        };
      };

      config = lib.mkIf cfg.enable {
        sops.secrets = lib.genAttrs (builtins.attrValues secretVars) (_: { }) // {
          # see "Name resolution" below
          trappist_hosts.restartUnits = [ "trappist-dns.service" ];
          trappist_dns_domain = { };
        };

        sops.templates."networkmanager-trappist-env-file".content = lib.concatMapStrings (
          var: "${var}=${config.sops.placeholder.${secretVars.${var}}}\n"
        ) (builtins.attrNames secretVars);

        networking.networkmanager.ensureProfiles = {
          environmentFiles = [
            config.sops.templates."networkmanager-trappist-env-file".path
          ];

          profiles.${cfg.interfaceName} = {
            connection = {
              id = cfg.interfaceName;
              type = "wireguard";
              interface-name = cfg.interfaceName;
              autoconnect = true;
            };
            wireguard = {
              private-key = "$TRAPPIST_WG_PRIVATE_KEY";
              mtu = cfg.mtu;
            };
            "wireguard-peer.$TRAPPIST_WG_PEER_PUBLIC_KEY" = {
              endpoint = "$TRAPPIST_WG_ENDPOINT";
              allowed-ips = "$TRAPPIST_WG_ALLOWED_IPS;";
              persistent-keepalive = cfg.persistentKeepalive;
            };
            ipv4 = {
              method = "manual";
              address1 = "$TRAPPIST_WG_ADDRESS";
            };
            ipv6.method = "disabled";
          };
        };

        # Name resolution for trappist services. Both the zone and the
        # host -> IP mapping live in sops; nothing about them is in git.
        #
        #   resolved --(dns-delegate, zone from sops)--> trappist-dns (dnsmasq)
        #                                                  `- addn-hosts from sops
        #
        # /etc/hosts stays fully NixOS-managed. If sops fails to decrypt, the
        # delegate file is never rendered and trappist-dns fails to load its
        # credential, so only the trappist names stop resolving.
        sops.templates."trappist.dns-delegate" = {
          path = "/run/systemd/dns-delegate.d/trappist.dns-delegate";
          mode = "0444";
          content = ''
            [Delegate]
            DNS=${cfg.dnsListenAddress}
            Domains=~${config.sops.placeholder.trappist_dns_domain}
          '';
          restartUnits = [ "systemd-resolved.service" ];
        };

        systemd.services.trappist-dns =
          let
            conf = pkgs.writeText "trappist-dnsmasq.conf" ''
              listen-address=${cfg.dnsListenAddress}
              bind-interfaces
              port=53
              no-resolv
              no-hosts
              no-negcache
              pid-file=
              addn-hosts=/run/credentials/trappist-dns.service/hosts
            '';
          in
          {
            description = "dnsmasq serving trappist host names to systemd-resolved";
            wantedBy = [ "multi-user.target" ];
            before = [ "nss-lookup.target" ];
            wants = [ "nss-lookup.target" ];
            serviceConfig = {
              ExecStart = "${pkgs.dnsmasq}/bin/dnsmasq --keep-in-foreground --conf-file=${conf}";
              LoadCredential = [ "hosts:${config.sops.secrets.trappist_hosts.path}" ];
              DynamicUser = true;
              AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
              CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
              RestrictAddressFamilies = [
                "AF_INET"
                "AF_INET6"
                "AF_UNIX"
                "AF_NETLINK"
              ];
              Restart = "on-failure";
              RestartSec = 5;
              ProtectSystem = "strict";
              ProtectHome = true;
              PrivateTmp = true;
              PrivateDevices = true;
              NoNewPrivileges = true;
            };
          };

        environment.systemPackages = [ pkgs.wireguard-tools ];
      };
    };
}
