{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.caddy-with-cloudflare-dns = pkgs.caddy.withPlugins {
        plugins = [
          "github.com/caddy-dns/cloudflare@v0.0.0-20251022184029-2fc25ee62f40"
        ];
        hash = "sha256-sexPn0LzErmK8ptUICUPSSqLNLYIy7F9M3JBJfyCpJQ";
      };
    };
}
