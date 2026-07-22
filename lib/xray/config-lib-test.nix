# Pure evaluation test: `nix eval --file modules/xray/config-lib-test.nix`
# Returns "ok" on success; throws (assertion) on failure.
let
  x = import ./config-lib.nix;

  front = x.mkFrontSettings {
    userIds = [ "UUID1" "UUID2" ];
    linkId = "LINK";
    frontDest = "aws.amazon.com:443";
    frontSni = "aws.amazon.com";
    frontPrivateKey = "PRIV";
    frontShortId = "sid";
    exitEndpoint = "home.example.org";
    exitPort = "443";
    exitSni = "www.microsoft.com";
    exitPublicKey = "EPUB";
    exitShortId = "esid";
  };
  frontIn = builtins.head front.inbounds;
  frontOut = builtins.head front.outbounds;

  exit = x.mkExitSettings {
    linkId = "LINK";
    exitDest = "www.microsoft.com:443";
    exitSni = "www.microsoft.com";
    exitPrivateKey = "EPRIV";
    exitShortId = "esid";
  };
  exitIn = builtins.head exit.inbounds;

  client = x.mkClientSettings {
    userId = "UUID1";
    frontEndpoint = "aws.example.org";
    frontPort = "443";
    frontSni = "aws.amazon.com";
    frontPublicKey = "FPUB";
    frontShortId = "sid";
    bypassGeosite = "geosite:category-ru";
    bypassGeoip = "geoip:ru";
  };
  proxyOut = builtins.head (builtins.filter (o: o.tag == "proxy") client.outbounds);
  tags = builtins.map (o: o.tag) client.outbounds;
  directRules = builtins.filter (r: r.outboundTag == "direct") client.routing.rules;

  checks = [
    (x.userNames == [ "xr1" "xr2" ])
    # front inbound: vless on 443, reality, two clients each with vision flow
    (frontIn.port == 443)
    (frontIn.protocol == "vless")
    (frontIn.streamSettings.security == "reality")
    (frontIn.streamSettings.realitySettings.dest == "aws.amazon.com:443")
    (frontIn.streamSettings.realitySettings.serverNames == [ "aws.amazon.com" ])
    (frontIn.streamSettings.realitySettings.privateKey == "PRIV")
    (frontIn.streamSettings.realitySettings.shortIds == [ "sid" ])
    (builtins.length frontIn.settings.clients == 2)
    ((builtins.head frontIn.settings.clients).flow == "xtls-rprx-vision")
    (frontIn.settings.decryption == "none")
    # front outbound: vless to exit, reality client params, vision flow
    (frontOut.tag == "to-exit")
    ((builtins.head frontOut.settings.vnext).address == "home.example.org")
    ((builtins.head frontOut.settings.vnext).port == "443")
    ((builtins.head (builtins.head frontOut.settings.vnext).users).id == "LINK")
    (frontOut.streamSettings.realitySettings.publicKey == "EPUB")
    (frontOut.streamSettings.realitySettings.serverName == "www.microsoft.com")
    (frontOut.streamSettings.realitySettings.shortId == "esid")
    (frontOut.streamSettings.realitySettings.fingerprint == "chrome")
    # front routing: everything to to-exit
    ((builtins.head front.routing.rules).outboundTag == "to-exit")
    # exit inbound: vless 8443 reality single client = link; freedom exit
    (exitIn.port == 8443)
    ((builtins.head exitIn.settings.clients).id == "LINK")
    (exitIn.streamSettings.realitySettings.privateKey == "EPRIV")
    ((builtins.head exit.outbounds).protocol == "freedom")
    ((builtins.head exit.outbounds).settings.domainStrategy == "UseIP")
    # client: 3 outbounds proxy/direct/block; split-tunnel bypass rules; dns present
    (tags == [ "proxy" "direct" "block" ])
    (proxyOut.streamSettings.realitySettings.publicKey == "FPUB")
    ((builtins.head proxyOut.settings.vnext).port == "443")
    (client.routing.domainStrategy == "IPIfNonMatch")
    (builtins.any (r: r.domain or [] == [ "geosite:category-ru" ]) directRules)
    (builtins.any (r: builtins.elem "geoip:ru" (r.ip or [])) directRules)
    (builtins.any (r: builtins.elem "geoip:private" (r.ip or [])) directRules)
    (builtins.length client.dns.servers >= 2)
  ];
  failed = builtins.filter (c: c != true) checks;
in
if failed == [ ] then "ok" else throw "config-lib-test: ${toString (builtins.length failed)} check(s) failed"
