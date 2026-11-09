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

        hosts = lib.mkOption {
          type = lib.types.attrsOf (lib.types.listOf lib.types.str);
          default = {
            "10.111.0.11" = [
              "grafana.trappist.internal"
              "wazuh.trappist.internal"
            ];
          };
          description = ''
            `/etc/hosts` entries for trappist services. The trappist network runs
            no resolver and pushes none to its clients, so names are resolved
            statically. These two names are explicitly allowed in plaintext;
            everything else about the network stays in sops.
          '';
        };
      };

      config = lib.mkIf cfg.enable {
        sops.secrets = lib.genAttrs (builtins.attrValues secretVars) (_: { });

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

        networking.hosts = cfg.hosts;

        environment.systemPackages = [ pkgs.wireguard-tools ];
      };
    };
}
