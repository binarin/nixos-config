{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.caddy-with-cloudflare-dns = pkgs.caddy.withPlugins {
        plugins = [
          "github.com/caddy-dns/cloudflare@v0.0.0-20251022184029-2fc25ee62f40"
        ];

        hash = "sha256-/dfWWDfPxFn30lyzJubu3s8kOySdosu8kc3dqkFvRH4=";

        # hash = "sha256-0iTaiuBwJfo8pUBvkAhVqcTEH4in1SZsTUJcdpnm/tk=";
      };
    };
}
