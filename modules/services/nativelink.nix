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
              experimental_cloud_object_store: {
                provider: "aws",
                region: "garage",
                bucket: "nativelink-cache",
                key_prefix: "cas/",
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
              listener: { http: { socket_address: "0.0.0.0:50051" } },
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
                    # The nix2container image ships only /bin — no CA trust store.
                    # Mount one so aws-sdk-rust (rustls) can verify Garage's TLS.
                    "${pkgs.cacert}/etc/ssl/certs:/etc/ssl/certs:ro"
                  ];
                  # Secret AWS_* creds come from the clan var env file; the
                  # non-secret settings are inline.
                  env_file = [ config.clan.core.vars.generators.nativelink-s3.files.env.path ];
                  environment = {
                    AWS_DEFAULT_REGION = "garage";
                    AWS_ENDPOINT_URL = "https://s3.lynx-lizard.ts.net";
                    AWS_REQUEST_CHECKSUM_CALCULATION = "WHEN_REQUIRED";
                    AWS_RESPONSE_CHECKSUM_VALIDATION = "WHEN_REQUIRED";
                    SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";
                    SSL_CERT_DIR = "/etc/ssl/certs";
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
