{ self, ... }:
{
  # VM test of use-nix-cache's local proxy: the ts.net fallback, the probe endpoints
  # and the probe's textfile metric. `primary` stands in for garage, `tsnet` for the
  # ts.net name; dnsmasq on 100.100.100.100 stands in for tailscaled's MagicDNS.
  perSystem =
    { pkgs, lib, ... }:
    let
      tlsCert = pkgs.runCommand "niks3-storage-test-cert" { nativeBuildInputs = [ pkgs.openssl ]; } ''
        mkdir $out
        openssl req -x509 -newkey rsa:2048 -nodes -days 3650 \
          -subj /CN=niks3-storage.lynx-lizard.ts.net \
          -keyout $out/key.pem -out $out/cert.pem
      '';

      # A TLS vhost of a fake cache upstream that names itself in its answers.
      cacheVhost = name: {
        onlySSL = true;
        sslCertificate = "${tlsCert}/cert.pem";
        sslCertificateKey = "${tlsCert}/key.pem";
        locations."= /nix-cache-info".return = ''200 "StoreDir: /nix/store\nWantMassQuery: 1\n"'';
        locations."= /present.narinfo".return = ''200 "from ${name}\n"'';
      };
    in
    {
      checks = lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
        use-nix-cache = pkgs.testers.runNixOSTest {
          name = "use-nix-cache";

          nodes.primary =
            { lib, ... }:
            {
              networking.firewall.allowedTCPPorts = [ 443 ];
              services.nginx = {
                enable = true;
                virtualHosts."niks3-storage.lynx-lizard.ts.net" = lib.recursiveUpdate (cacheVhost "primary") {
                  # Like garage's caddy when the store behind it is down.
                  locations."= /bad-gateway.narinfo".return = "502";
                };
              };
              # Like garage's caddy before SL-0062: no TLS site for the ts.net name,
              # so the handshake with that SNI is refused.
              specialisation.wrong-sni.configuration = {
                services.nginx.virtualHosts = lib.mkForce {
                  "niks3-storage.home.binarin.info" = cacheVhost "primary";
                  "reject" = {
                    default = true;
                    rejectSSL = true;
                  };
                };
              };
            };

          nodes.tsnet =
            { lib, ... }:
            {
              networking.firewall.allowedTCPPorts = [ 443 ];
              services.nginx = {
                enable = true;
                virtualHosts."niks3-storage.lynx-lizard.ts.net" = lib.recursiveUpdate (cacheVhost "tsnet") {
                  # Served here only, to show that a 404 or 502 from the primary is
                  # (not) retried.
                  locations."= /missing.narinfo".return = ''200 "from tsnet\n"'';
                  locations."= /bad-gateway.narinfo".return = ''200 "from tsnet\n"'';
                };
              };
            };

          nodes.client =
            { nodes, lib, ... }:
            {
              imports = [
                self.nixosModules.monitored
                self.nixosModules.use-nix-cache
              ];
              nixos-config.export-metrics.enable = true;
              services.vmagent.enable = lib.mkForce false;

              networking.hosts.${nodes.primary.networking.primaryIPAddress} = [
                "niks3-storage.home.binarin.info"
              ];

              systemd.services.magicdns-address = {
                requiredBy = [ "dnsmasq.service" ];
                before = [ "dnsmasq.service" ];
                serviceConfig = {
                  Type = "oneshot";
                  RemainAfterExit = true;
                };
                script = "${lib.getExe' pkgs.iproute2 "ip"} address add 100.100.100.100/32 dev lo";
              };
              services.dnsmasq = {
                enable = true;
                resolveLocalQueries = false;
                settings = {
                  listen-address = "100.100.100.100";
                  bind-interfaces = true;
                  no-resolv = true;
                  address = "/niks3-storage.lynx-lizard.ts.net/${nodes.tsnet.networking.primaryIPAddress}";
                };
              };
            };

          testScript = ''
            start_all()
            for upstream in (primary, tsnet):
                upstream.wait_for_unit("nginx.service")
                upstream.wait_for_open_port(443)
            client.wait_for_unit("nginx.service")
            client.wait_for_unit("dnsmasq.service")
            client.wait_for_unit("prometheus-node-exporter.service")
            client.succeed("systemctl is-active nix-cache-upstream-probe.timer")

            textfile = "/var/lib/prometheus-node-exporter-text-files/nix-cache-upstream.prom"

            def get(path):
                status, upstream = client.succeed(
                    "curl -s -o /tmp/body --max-time 30 -w '%{http_code} %header{x-nix-cache-upstream}' "
                    f"http://niks3-storage.nix-cache:48080{path}"
                ).split(" ", 1)
                return status, upstream.strip(), client.succeed("cat /tmp/body")

            def expect(path, status, upstream, body=None):
                got = get(path)
                assert got[:2] == (status, upstream), f"{path}: got {got}, want {status} {upstream}"
                if body is not None:
                    assert body in got[2], f"{path}: body {got[2]!r} lacks {body!r}"

            def expect_probe(primary_ok, tsnet_ok):
                client.succeed("systemctl start nix-cache-upstream-probe.service")
                text = client.succeed(f"cat {textfile}")
                metrics = client.succeed("curl -sf http://127.0.0.1:9100/metrics")
                for line in (
                    f'nix_cache_upstream_ok{{upstream="primary"}} {primary_ok}',
                    f'nix_cache_upstream_ok{{upstream="tsnet"}} {tsnet_ok}',
                ):
                    assert line in text, f"{textfile} lacks {line!r}: {text}"
                    assert line in metrics, f"node exporter lacks {line!r}"
                assert "nix_cache_upstream_probe_timestamp_seconds " in metrics
                # Atomic write: only the final file, readable by node exporter.
                client.succeed(
                    f"test \"$(ls -A $(dirname {textfile}))\" = $(basename {textfile})",
                    f"test \"$(stat -c '%a %G' {textfile})\" = '644 node-exporter-textfile'",
                )

            with subtest("primary answers"):
                expect("/present.narinfo", "200", "primary", "from primary")
                expect("/.upstream-nix-cache-info", "200", "primary", "StoreDir: /nix/store")
                expect("/.upstream-nix-cache-info/tsnet", "200", "tsnet", "StoreDir: /nix/store")
                # Answered locally, never proxied.
                expect("/nix-cache-info", "200", "", "Priority: 41")
                expect_probe(1, 1)

            with subtest("a real 404 from the primary is not retried"):
                expect("/missing.narinfo", "404", "primary")

            with subtest("a 502 sent by the primary is retried at ts.net"):
                expect("/bad-gateway.narinfo", "200", "tsnet", "from tsnet")

            with subtest("primary refuses the ts.net SNI (SL-0062)"):
                primary.succeed("/run/booted-system/specialisation/wrong-sni/bin/switch-to-configuration test")
                expect("/present.narinfo", "200", "tsnet", "from tsnet")
                expect("/.upstream-nix-cache-info", "200", "tsnet", "StoreDir: /nix/store")
                # ts.net's own 404 stays a 404.
                expect("/absent.narinfo", "404", "tsnet")
                expect_probe(0, 1)
                primary.succeed("/run/booted-system/bin/switch-to-configuration test")
                expect("/present.narinfo", "200", "primary", "from primary")

            with subtest("primary refuses connections"):
                primary.succeed("systemctl stop nginx.service")
                expect("/present.narinfo", "200", "tsnet", "from tsnet")
                expect_probe(0, 1)

            with subtest("both refuse connections"):
                tsnet.succeed("systemctl stop nginx.service")
                expect("/present.narinfo", "502", "none")
                expect("/.upstream-nix-cache-info", "502", "none")
                expect("/.upstream-nix-cache-info/tsnet", "502", "none")
                expect_probe(0, 0)
                for upstream in (primary, tsnet):
                    upstream.succeed("systemctl start nginx.service")
                    upstream.wait_for_open_port(443)

            with subtest("primary times out"):
                primary.succeed("iptables -I INPUT -p tcp --dport 443 -j DROP")
                expect("/present.narinfo", "200", "tsnet", "from tsnet")

            with subtest("both time out: ts.net's 504 becomes 502"):
                tsnet.succeed("iptables -I INPUT -p tcp --dport 443 -j DROP")
                expect("/present.narinfo", "502", "none")
                for upstream in (primary, tsnet):
                    upstream.succeed("iptables -D INPUT -p tcp --dport 443 -j DROP")
                expect("/present.narinfo", "200", "primary", "from primary")

            with subtest("nginx starts without the ts.net resolver"):
                client.succeed("systemctl stop dnsmasq.service")
                client.succeed("systemctl restart nginx.service")
                client.wait_for_unit("nginx.service")
                client.wait_for_open_port(48080)
                expect("/present.narinfo", "200", "primary", "from primary")
                expect("/.upstream-nix-cache-info/tsnet", "502", "none")
                expect_probe(1, 0)
          '';
        };
      };
    };
}
