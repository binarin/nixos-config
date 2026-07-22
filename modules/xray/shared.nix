{ self, ... }:
{
  flake.nixosModules.xray-shared =
    { pkgs, ... }:
    let
      mkRealityGenerator = { }: {
        share = true;
        files.private-key.secret = true;
        files.public-key.secret = false;
        files.short-id.secret = false;
        runtimeInputs = [
          pkgs.xray
          pkgs.openssl
          pkgs.gnused
          pkgs.gnugrep
        ];
        script = ''
          keys="$(xray x25519 | sed 's/.*:[[:space:]]*//' | grep -v '^$')"
          printf '%s' "$(printf '%s\n' "$keys" | sed -n '1p')" > "$out/private-key"
          printf '%s' "$(printf '%s\n' "$keys" | sed -n '2p')" > "$out/public-key"
          openssl rand -hex 8 | tr -d '\n' > "$out/short-id"
        '';
      };
    in
    {
      key = "nixos-config.modules.nixos.xray-shared";
      config = {
        # --- REALITY keypairs (one per hop). Shared: same keypair on both
        # machines. private-key deploys everywhere it's imported (both our own
        # trusted machines); public-key/short-id are non-secret and readable in
        # Nix via `.value`. `xray x25519` output labels vary by version, so we
        # parse "value after the last colon" of the first two non-empty lines.
        clan.core.vars.generators.xray-reality-front = mkRealityGenerator { };
        clan.core.vars.generators.xray-reality-exit = mkRealityGenerator { };

        # --- Server-to-server VLESS credential for the front->exit hop.
        clan.core.vars.generators.xray-link = {
          share = true;
          files.uuid.secret = true;
          runtimeInputs = [ pkgs.xray ];
          script = ''
            xray uuid | tr -d '\n' > "$out/uuid"
          '';
        };

        # --- Identifying params (secret). sni/dest/port have baked defaults you
        # can later override with `clan vars set`; endpoint is prompted (only you
        # know the EC2 address / DDNS host). Front endpoint = client-dialed EC2
        # address; exit endpoint = the DDNS host the front dials.
        clan.core.vars.generators.xray-front-params = {
          share = true;
          prompts.endpoint.description = "xray-front public host that CLIENTS dial (Elastic IP or DNS)";
          files.endpoint.secret = true;
          files.sni.secret = true;
          files.dest.secret = true;
          files.port.secret = true;
          script = ''
            cat "$prompts/endpoint" | tr -d '\n' > "$out/endpoint"
            printf 'aws.amazon.com'      > "$out/sni"
            printf 'aws.amazon.com:443'  > "$out/dest"
            printf '443'                 > "$out/port"
          '';
        };

        clan.core.vars.generators.xray-exit-params = {
          share = true;
          prompts.endpoint.description = "home DDNS host that xray-front dials to reach xray-exit";
          files.endpoint.secret = true;
          files.sni.secret = true;
          files.dest.secret = true;
          files.port.secret = true;
          script = ''
            cat "$prompts/endpoint" | tr -d '\n' > "$out/endpoint"
            printf 'www.microsoft.com'     > "$out/sni"
            printf 'www.microsoft.com:443' > "$out/dest"
            printf '443'                   > "$out/port"
          '';
        };

        clan.core.vars.generators.xray-geo = {
          share = true;
          files.bypass-geosite.secret = true;
          files.bypass-geoip.secret = true;
          script = ''
            printf 'geosite:category-ru' > "$out/bypass-geosite"
            printf 'geoip:ru'            > "$out/bypass-geoip"
          '';
        };
      };
    };
}
