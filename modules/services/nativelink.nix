{
  inputs,
  self,
  config,
  ...
}:
let
  imageTags = builtins.fromJSON (builtins.readFile ./nativelink.json);
in
{
  # ghcr pivot: NO flake input. The pinned upstream image is referenced directly
  # (tag+digest in ./nativelink.json), so NativeLink adds nothing to nix eval.
  # Any prior `flake-file.inputs.nativelink` is intentionally gone; regenerate
  # flake.nix + relock to drop `nativelink` from flake.lock.

  flake.nixosModules.nativelink =
    { config, pkgs, lib, ... }:
    let
      # Cache-only config: CAS + AC over fast_slow(redis, garage-s3).
      # Structure lifted from nativelink-config/examples/redis.json5 (already
      # cache-only) with the redis-only tiers swapped for fast_slow(redis, s3).
      configFile = pkgs.writeText "nativelink-config.json5" ''
        {
          stores: [
            {
              name: "REDIS_CAS",
              redis_store: {
                addresses: [ "redis://redis:6379/" ],
                key_prefix: "cas:",
              },
            },
            {
              name: "REDIS_AC",
              redis_store: {
                addresses: [ "redis://redis:6379/" ],
                key_prefix: "ac:",
              },
            },
            {
              name: "S3_CAS",
              // Garage needs PATH-STYLE addressing (bucket in the path, not a
              // <bucket>.host subdomain — MagicDNS won't resolve that). NativeLink's
              // aws provider has no path-style toggle, BUT an IP endpoint forces
              // path-style (the SDK can't prepend a bucket to an IP). We also need the
              // SDK's default flexible checksum OFF (Garage rejects it) — the aws
              // provider honors AWS_REQUEST_CHECKSUM_CALCULATION=WHEN_REQUIRED, which
              // the ontap provider ignored. Endpoint (garage tailscale IP :3900, raw
              // S3 API, plaintext over WireGuard) comes from AWS_ENDPOINT_URL.
              experimental_cloud_object_store: {
                provider: "aws",
                region: "garage",
                bucket: "nativelink-cache",
                key_prefix: "cas/",
                insecure_allow_http: true,
                retry: { max_retries: 6, delay: 0.3, jitter: 0.5 },
              },
            },
            {
              name: "S3_AC",
              experimental_cloud_object_store: {
                provider: "aws",
                region: "garage",
                bucket: "nativelink-cache",
                key_prefix: "ac/",
                insecure_allow_http: true,
                retry: { max_retries: 6, delay: 0.3, jitter: 0.5 },
              },
            },
            {
              name: "CAS_FAST_SLOW",
              fast_slow: {
                fast: { ref_store: { name: "REDIS_CAS" } },
                slow: { ref_store: { name: "S3_CAS" } },
              },
            },
            {
              name: "AC_FAST_SLOW",
              fast_slow: {
                fast: { ref_store: { name: "REDIS_AC" } },
                slow: { ref_store: { name: "S3_AC" } },
              },
            },
            {
              name: "CAS_MAIN_STORE",
              existence_cache: {
                backend: {
                  compression: {
                    compression_algorithm: { lz4: {} },
                    backend: { ref_store: { name: "CAS_FAST_SLOW" } },
                  },
                },
              },
            },
            {
              name: "AC_MAIN_STORE",
              completeness_checking: {
                backend: { ref_store: { name: "AC_FAST_SLOW" } },
                cas_store: { ref_store: { name: "CAS_MAIN_STORE" } },
              },
            },
          ],
          servers: [
            {
              listener: {
                http: {
                  socket_address: "0.0.0.0:50051",
                  // NativeLink terminates TLS itself (rustls negotiates h2 ALPN, which
                  // tailscale's tls-terminated-tcp cannot). Cert minted by the
                  // nativelink-tls-cert.service unit; vip is a raw tcp passthrough.
                  tls: {
                    cert_file: "/etc/nativelink/tls/cert.pem",
                    key_file: "/etc/nativelink/tls/key.pem",
                  },
                },
              },
              services: {
                cas: [ { instance_name: "main", cas_store: "CAS_MAIN_STORE" } ],
                ac: [ { instance_name: "main", ac_store: "AC_MAIN_STORE" } ],
                bytestream: [ { instance_name: "main", cas_store: "CAS_MAIN_STORE" } ],
                // Cache-only capabilities (no remote_execution) — required so Bazel's
                // GetCapabilities call succeeds and it actually uses the cache.
                capabilities: [ { instance_name: "main" } ],
                health: {},
              },
            },
          ],
        }
      '';
    in
    {
      key = "nixos-config.modules.nixos.nativelink";

      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.arion.nixosModules.arion
        self.nixosModules.tailscale
      ];

      config = {
        # Dedicated Garage S3 key for NativeLink, provisioned via clan vars
        # (docker-on-nixos is a clan machine). The generator emits a ready-to-source
        # env file holding just the two secret AWS_* lines; Arion consumes it via
        # `env_file`. We reference the generator's file PATH directly (the niks3
        # pattern) rather than a sops.placeholder — the placeholder does not exist
        # at eval time before `clan vars generate` has run. Provision with:
        #   clan vars generate docker-on-nixos
        # NOTE: rotating the key (re-running the generator) does not auto-restart the
        # container — the env_file path is stable — so restart nativelink manually.
        clan.core.vars.generators.nativelink-s3 = {
          prompts.access-key.description = "Garage S3 key id (nativelink-cache bucket)";
          prompts.secret-key.description = "Garage S3 secret key (nativelink-cache bucket)";
          files.env = {
            secret = true;
            deploy = true;
          };
          script = ''
            {
              printf 'AWS_ACCESS_KEY_ID=%s\n' "$(tr -d '\n' < "$prompts/access-key")"
              printf 'AWS_SECRET_ACCESS_KEY=%s\n' "$(tr -d '\n' < "$prompts/secret-key")"
            } > "$out/env"
          '';
        };

        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.nativelink = {
          serviceName = "nativelink-docker-compose";
          settings = {
            services = {
              nativelink = {
                service = {
                  # Pinned upstream ghcr image (no nix build). The image entrypoint
                  # is the nativelink binary; the config path is passed as its arg.
                  image = "ghcr.io/tracemachina/nativelink:${imageTags.nativelink}";
                  command = [ "/etc/nativelink/config.json5" ];
                  container_name = "nativelink";
                  restart = "unless-stopped";
                  # Publish gRPC only to host-localhost; tailscale serve fronts it.
                  ports = [ "127.0.0.1:50051:50051" ];
                  volumes = [
                    "${configFile}:/etc/nativelink/config.json5:ro"
                    # tailscale-issued TLS cert for bazel-cache.lynx-lizard.ts.net.
                    "/var/lib/nativelink-tls:/etc/nativelink/tls:ro"
                  ];
                  # Secret AWS_* creds come from the clan var env file; the
                  # non-secret settings are inline.
                  env_file = [ config.clan.core.vars.generators.nativelink-s3.files.env.path ];
                  environment = {
                    AWS_DEFAULT_REGION = "garage";
                    # Garage's raw S3 API on the garage node's TAILSCALE IP. An IP
                    # endpoint forces path-style addressing (see S3 store comment);
                    # plaintext http, but WireGuard-encrypted on the tailnet. Not
                    # inventory-sourced: inventory.ipAllocation carries only LAN addrs
                    # and garage's :3900 is firewalled off the LAN (tailscale-only).
                    # Stable per tailnet node; update if garage leaves/rejoins the net.
                    AWS_ENDPOINT_URL = "http://100.67.195.61:3900";
                    AWS_REQUEST_CHECKSUM_CALCULATION = "WHEN_REQUIRED";
                    AWS_RESPONSE_CHECKSUM_VALIDATION = "WHEN_REQUIRED";
                    RUST_LOG = "info";
                  };
                  depends_on = [ "redis" ];
                };
              };
              redis = {
                service = {
                  image = "redis:${imageTags.redis}";
                  container_name = "nativelink-redis";
                  restart = "unless-stopped";
                  # Internal only: no `ports`. Reachable by nativelink as redis:6379.
                  expose = [ "6379" ];
                };
              };
            };
          };
        };

        # Mint/renew the TLS cert for the vip-service name via `tailscale cert`,
        # before the container starts, and restart the cache only when the cert
        # actually changes (so weekly renewal checks don't cause needless restarts).
        # /var/lib/nativelink-tls is ephemeral under impermanence and re-minted each
        # boot — this does NOT re-hit LetsEncrypt only because tailscale's own cert
        # cache is persisted (impermanence bind-mounts /var/lib/tailscale). If that
        # ever changes, persist /var/lib/nativelink-tls to avoid LE rate limits.
        systemd.services.nativelink-tls-cert = {
          description = "Fetch/renew tailscale TLS cert for nativelink (bazel-cache)";
          after = [ "tailscaled.service" ];
          wants = [ "tailscaled.service" ];
          before = [ "nativelink-docker-compose.service" ];
          wantedBy = [ "nativelink-docker-compose.service" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            StateDirectory = "nativelink-tls";
            StateDirectoryMode = "0700";
          };
          script = ''
            set -euo pipefail
            tmp=$(mktemp -d)
            trap 'rm -rf "$tmp"' EXIT
            ${config.services.tailscale.package}/bin/tailscale cert \
              --cert-file "$tmp/cert.pem" --key-file "$tmp/key.pem" \
              bazel-cache.lynx-lizard.ts.net
            if ! cmp -s "$tmp/cert.pem" /var/lib/nativelink-tls/cert.pem 2>/dev/null; then
              install -m0644 "$tmp/cert.pem" /var/lib/nativelink-tls/cert.pem
              install -m0600 "$tmp/key.pem" /var/lib/nativelink-tls/key.pem
              ${pkgs.systemd}/bin/systemctl try-restart nativelink-docker-compose.service
            fi
          '';
        };
        systemd.timers.nativelink-tls-cert = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "weekly";
            Persistent = true;
          };
        };

        # Expose as the pre-allocated `bazel-cache` Tailscale Service (vip-service).
        # Raw `tcp` passthrough (NOT tls-terminated-tcp): the tailscale TLS terminator
        # does not negotiate h2 ALPN, which gRPC-over-TLS requires. Clients use plaintext
        # grpc:// (still WireGuard-encrypted on the wire) → h2c to localhost:50051.
        # Rationale: Tailscale free-plan machine cap → advertise a service, not a
        # new tailnet node. A 2nd backend elsewhere → automatic LB (stateless tier).
        services.tailscale.serve.enable = lib.mkDefault true;
        services.tailscale.serve.services.bazel-cache = {
          serviceName = "bazel-cache";
          protocol = "tcp";
          port = 443;
          target = "localhost:50051";
        };
      };
    };
}
