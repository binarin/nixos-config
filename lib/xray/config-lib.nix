# Pure, lib-free xray config builders. Import with:
#   xrayLib = import "${self}/lib/xray/config-lib.nix";
# All leaf string params are value-agnostic: real values, sops placeholders,
# or @SENTINEL@ tokens all pass straight through into the produced attrset.
rec {
  userNames = [ "xr1" "xr2" ];

  # REALITY "raw" (TCP) stream — server side.
  realityServer =
    { dest, serverNames, privateKey, shortIds }:
    {
      network = "raw";
      security = "reality";
      realitySettings = {
        show = false;
        dest = dest;
        xver = 0;
        serverNames = serverNames;
        privateKey = privateKey;
        shortIds = shortIds;
      };
    };

  # REALITY "raw" (TCP) stream — client side (uTLS chrome fingerprint).
  realityClient =
    { serverName, publicKey, shortId }:
    {
      network = "raw";
      security = "reality";
      realitySettings = {
        serverName = serverName;
        fingerprint = "chrome";
        publicKey = publicKey;
        shortId = shortId;
        spiderX = "/";
      };
    };

  mkFrontSettings =
    { userIds, linkId, frontDest, frontSni, frontPrivateKey, frontShortId
    , exitEndpoint, exitPort, exitSni, exitPublicKey, exitShortId }:
    {
      log.loglevel = "warning";
      inbounds = [
        {
          tag = "in";
          listen = "0.0.0.0";
          port = 443;
          protocol = "vless";
          settings = {
            clients = builtins.map (id: { id = id; flow = "xtls-rprx-vision"; }) userIds;
            decryption = "none";
          };
          streamSettings = realityServer {
            dest = frontDest;
            serverNames = [ frontSni ];
            privateKey = frontPrivateKey;
            shortIds = [ frontShortId ];
          };
        }
      ];
      outbounds = [
        {
          tag = "to-exit";
          protocol = "vless";
          settings.vnext = [
            {
              address = exitEndpoint;
              port = exitPort;
              users = [ { id = linkId; flow = "xtls-rprx-vision"; encryption = "none"; } ];
            }
          ];
          streamSettings = realityClient {
            serverName = exitSni;
            publicKey = exitPublicKey;
            shortId = exitShortId;
          };
        }
      ];
      routing = {
        domainStrategy = "AsIs";
        rules = [ { type = "field"; inboundTag = [ "in" ]; outboundTag = "to-exit"; } ];
      };
    };

  mkExitSettings =
    { linkId, exitDest, exitSni, exitPrivateKey, exitShortId }:
    {
      log.loglevel = "warning";
      inbounds = [
        {
          tag = "in";
          listen = "0.0.0.0";
          port = 8443;
          protocol = "vless";
          settings = {
            clients = [ { id = linkId; flow = "xtls-rprx-vision"; } ];
            decryption = "none";
          };
          streamSettings = realityServer {
            dest = exitDest;
            serverNames = [ exitSni ];
            privateKey = exitPrivateKey;
            shortIds = [ exitShortId ];
          };
        }
      ];
      outbounds = [ { tag = "exit"; protocol = "freedom"; settings.domainStrategy = "UseIP"; } ];
      routing = {
        domainStrategy = "AsIs";
        rules = [ { type = "field"; inboundTag = [ "in" ]; outboundTag = "exit"; } ];
      };
    };

  mkClientSettings =
    { userId, frontEndpoint, frontPort, frontSni, frontPublicKey, frontShortId
    , bypassGeosite, bypassGeoip }:
    {
      log.loglevel = "warning";
      inbounds = [
        { tag = "socks"; listen = "127.0.0.1"; port = 10808; protocol = "socks"; settings = { udp = true; }; }
        { tag = "http"; listen = "127.0.0.1"; port = 10809; protocol = "http"; settings = { }; }
      ];
      outbounds = [
        {
          tag = "proxy";
          protocol = "vless";
          settings.vnext = [
            {
              address = frontEndpoint;
              port = frontPort;
              users = [ { id = userId; flow = "xtls-rprx-vision"; encryption = "none"; } ];
            }
          ];
          streamSettings = realityClient {
            serverName = frontSni;
            publicKey = frontPublicKey;
            shortId = frontShortId;
          };
        }
        { tag = "direct"; protocol = "freedom"; settings.domainStrategy = "UseIP"; }
        { tag = "block"; protocol = "blackhole"; settings = { }; }
      ];
      routing = {
        domainStrategy = "IPIfNonMatch";
        rules = [
          { type = "field"; outboundTag = "direct"; domain = [ bypassGeosite ]; }
          { type = "field"; outboundTag = "direct"; ip = [ bypassGeoip "geoip:private" ]; }
          { type = "field"; outboundTag = "direct"; ip = [ "127.0.0.1/8" "::1/128" ]; }
        ];
      };
      dns = {
        servers = [
          "https://1.1.1.1/dns-query"
          { address = "77.88.8.8"; domains = [ bypassGeosite ]; }
        ];
      };
    };
}
