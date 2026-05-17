{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.caddy-with-cloudflare-dns = pkgs.caddy.withPlugins {
        plugins = [
          "github.com/caddy-dns/cloudflare@v0.0.0-20251022184029-2fc25ee62f40"
        ];

        hash = "sha256-0f1ATcCQviQff6MsrPSldLQhX0Yp5vW3gdGTkWDcy8s=";
      };
    };
}
