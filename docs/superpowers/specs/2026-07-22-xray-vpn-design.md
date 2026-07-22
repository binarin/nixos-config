# Xray two-hop residential-exit VPN — design

Date: 2026-07-22
Status: approved (brainstorming), pending implementation plan

## Goal

Stand up an anti-censorship VPN, tuned for use **from Russia**, that gives a
small set of users (2–3) a **residential exit** through the operator's home
connection. Two new clan-managed NixOS machines:

- **`xray-front`** — a small AWS EC2 VM. Public entry point. Terminates the
  user-facing xray connection behind a REALITY "steal-oneself" camouflage
  (pretends to be a real AWS-family HTTPS site). Chains onward to the home node.
- **`xray-exit`** — a Proxmox VM at home on the guest VLAN (192.168.3.0/24).
  Provides the residential exit (`freedom` outbound → internet).

Routing between the two hops is handled by xray itself (chaining). Both machines
are managed by this nixos-config repo using the dendritic pattern + clan.

Start with two users, `xr1` and `xr2`.

## Non-goals

- No Tailscale on either new VM (explicit). Management is over plain SSH.
- No mobile-management UI, no per-user bandwidth accounting, no web dashboard.
- Not hiding the config *shape* from git — only identifying *values* are
  encrypted (see "Hiding identifying config").

## Background / key facts

- `pkgs.xray` (v26.2.6, XTLS/Xray-core) and the `services.xray` NixOS module
  already exist in nixpkgs. **No custom packaging/overlay needed.**
- Modern Russia-grade stack: **VLESS + `flow: xtls-rprx-vision` + REALITY**.
  - Per-user credential: VLESS `id` (UUID).
  - Per-server REALITY material: X25519 keypair (`private-key` on server,
    `public-key` on clients), a `short-id` pool, and `serverNames`/`dest`
    (the camouflage site). `xray x25519` generates the keypair; `xray uuid`
    generates UUIDs.
  - Client also sets `fingerprint: chrome` (uTLS) + `spiderX`.
- Chaining: an outbound with `proxySettings.tag` (or `sockopt.dialerProxy`)
  dials through another outbound; routing selects an outbound by `outboundTag`.
- Geo routing: `geosite:<code>` / `geoip:<code>` in rule `domain`/`ip` fields;
  routing-level `domainStrategy: IPIfNonMatch`; `freedom` outbound
  `domainStrategy: UseIP`.
- Repo conventions (from exploration):
  - Machines: one module per machine under `modules/machines/<name>.nix`,
    built through clan (`self.clan.nixosConfigurations.<name>`), re-exported as
    `flake.nixosConfigurations.<name>` with `inventoryHostName` in specialArgs.
    Disko per machine under `my-machines/<name>/disko.nix`.
  - Secrets: `clan.core.vars.generators.*` (sops-backed). Non-secret outputs are
    committed as plaintext `value`; secrets are sops-encrypted. Generators run
    on the operator's machine. Clan-vars secrets are addressable as sops
    placeholders `vars/<gen>/<file>`.
  - Rendering many secrets into one config file: `sops.templates."<name>"` +
    `config.sops.placeholder."vars/<gen>/<file>"`, point the service at the
    rendered path (metabase pattern) — secrets never enter `/nix/store`.
  - `qemu-guest` module's `40-qemu` systemd-networkd block is hardcoded to the
    **home** VLAN; the guest-VLAN VM overrides it in its own machine module.
  - `guest.toml` defines 192.168.3.0/24 (gw 192.168.3.1, `guest.binarin.info`).
  - New files must be `git add`-ed to be visible to the flake.
  - Overlays not needed here.

## Roles / operator model

Per the operator's working model: the assistant **writes config, commits, and
stages new files, then hands off**. The operator runs all live provisioning and
deploys (`clan machines update`, `clan vars generate`, `nixos-anywhere`, Proxmox
VM creation, router port-forward). The assistant may run `clan vars set` for
known non-personal values (camouflage/geo defaults) and may build/eval/lint, but
cannot run the live end-to-end test.

## Architecture

### Data flow

```
client (RU)                    xray-front (AWS)                 xray-exit (home)
  VLESS+Vision+REALITY   →   inbound :443 (REALITY,        →   inbound :8443 (REALITY,
  (camouflage SNI)           AWS-family camouflage)            generic camouflage)
                             route: all → outbound            reached via router
                             "to-exit"                        port-forward on DDNS host
                             outbound VLESS+REALITY  ───────→ → freedom outbound
                             dials <DDNS host>:<PORT>            = residential exit → internet
```

- **Censored/probed hop** is only client→`xray-front` (REALITY-camouflaged).
- **`xray-front`→`xray-exit`** hop: public port-forward to the operator's
  existing DDNS hostname, wrapped in a second VLESS+REALITY layer, authenticated
  with one server↔server credential. No Tailscale.
- Client decides *what* to tunnel (split-tunnel, below); the servers just chain.

### Machines

|                    | `xray-front` (AWS)                 | `xray-exit` (home)                   |
| ------------------ | ---------------------------------- | ------------------------------------ |
| Role               | Public entry, REALITY, chain→home  | Residential `freedom` exit           |
| Host               | EC2, nixos-anywhere + disko        | Proxmox VM, qemu-guest + disko       |
| Network            | EC2 public addr + DHCP             | Static IP on guest VLAN 192.168.3.x  |
| Deploy targetHost  | EC2 public DNS/IP (SSH)            | guest-VLAN LAN IP (SSH)              |
| Tailscale          | none                               | none                                 |
| Modules            | baseline, srvos-bits, amazon-image, disko | baseline, srvos-bits, qemu-guest (net override), disko |

Names are proposals; rename freely. The existing unused `xray` reservation at
192.168.2.14 is on the **home** VLAN, not the guest VLAN — leave it; allocate a
fresh guest-VLAN IP for `xray-exit`.

Ports (all overridable via vars): front inbound **443**, exit inbound **8443**,
forwarded public port operator's choice (config defaults to expect **443**).

### Networking specifics

- `xray-exit`: override `systemd.network.networks."40-qemu"` in the machine
  module to use the guest network (address from
  `inventory.ipAllocation.<host>.guest.primary.addressWithPrefix`, gateway
  192.168.3.1, guest DNS). Add the host to `inventory/networks/guest.toml` and
  `inventory/host-id.toml`.
- `xray-front`: own networking (EC2 DHCP + amazon-image profile,
  growPartition/EC2 metadata). Not on the home/guest inventory TOMLs (isolated
  machine); `deploy.targetHost` set to its public address. SSH locked to key +
  security group.

## Secrets model

Two kinds of clan var generators.

### Random crypto (not identifying)

| Generator            | Scope             | Files                                              |
| -------------------- | ----------------- | -------------------------------------------------- |
| `xray-reality-front` | per-machine front | `private-key` (secret), `public-key`, `short-id`   |
| `xray-reality-exit`  | per-machine exit  | `private-key` (secret), `public-key`, `short-id`   |
| `xray-link`          | shared            | `uuid` (secret) — server↔server credential         |
| `xray-user-xr1`      | per-user          | `uuid` (secret), `client.json` (secret) — see below|
| `xray-user-xr2`      | per-user          | `uuid` (secret), `client.json` (secret)            |

`runtimeInputs = [ pkgs.xray pkgs.openssl ]` where relevant. `public-key` and
`short-id` are non-secret (`secret = false`) — random, not identifying — so
cross-machine references use plain Nix `.value` interpolation.

### Identifying config (chosen values → secret, generically named)

| Generator          | Scope          | Secret files                                          |
| ------------------ | -------------- | ----------------------------------------------------- |
| `xray-front-params`| shared, secret | `sni`, `dest`, `endpoint` (client-dialed addr), `port`|
| `xray-exit-params` | shared, secret | `sni`, `dest`, `endpoint` (DDNS host), `port`         |
| `xray-geo`         | shared, secret | `bypass-geosite`, `bypass-geoip` (RU split tags)      |

- Generic names — nothing in git names AWS, Russia, or the operator's
  hostnames. Values are user-set: the assistant may `clan vars set` the
  recommended camouflage/geo defaults; the operator sets the real endpoints
  (DDNS host, front public address).
- These are `shared` so both machines and the operator (client generation) can
  read them.

### Recommended defaults (overridable, stored as secret values)

- `xray-front-params/sni` + `dest`: an AWS-family TLS 1.3 host (matches EC2 ASN),
  e.g. a real `*.amazon.com`-family HTTPS endpoint on :443. Final value chosen at
  implementation time against REALITY suitability (TLS 1.3, HTTP/2, X25519,
  stable, not operator-owned).
- `xray-exit-params/sni` + `dest`: a generic well-known TLS 1.3 host suitable for
  a residential IP.
- `xray-geo/bypass-geosite` = `geosite:category-ru` (+ optionally `geosite:private`),
  `xray-geo/bypass-geoip` = `geoip:ru` (+ `geoip:private`).

### Server config rendering (approach A)

Each server module builds its full `xray.json` **structure in Nix** and renders
it via `sops.templates."xray.json"`; every identifying string and every secret
is a `config.sops.placeholder."vars/…"` reference.
`services.xray.settingsFile = config.sops.templates."xray.json".path`. Restart
xray on change. Secrets never enter `/nix/store`; structure stays
diff-reviewable.

- `xray-front` `xray.json`: VLESS inbound :443 (Vision + REALITY, dest/sni from
  `xray-front-params`, `privateKey` from `xray-reality-front`, `shortIds` from
  `xray-reality-front`, `clients` = all `xray-user-*/uuid`). Outbound `to-exit` =
  VLESS+REALITY to `xray-exit-params/endpoint:port`, user =
  `xray-link/uuid`, REALITY `publicKey`/`shortId`/`serverName` from
  `xray-reality-exit`/`xray-exit-params`. Router: all → `to-exit`.
- `xray-exit` `xray.json`: VLESS inbound :8443 (Vision + REALITY, dest/sni from
  `xray-exit-params`, `privateKey` from `xray-reality-exit`, `clients` =
  `xray-link/uuid`). Outbound `freedom` (`domainStrategy: UseIP`) = residential
  exit. Router: all → `freedom`.

### Hiding identifying config — honest scope

Git reveals the config *shape* (a two-outbound split-tunnel client, a
camouflaged REALITY inbound chaining to a second REALITY hop) but never the
mimicked hosts, geo tags, endpoints, or ports — those are encrypted vars. Hiding
the shape itself would require opaque generated config blobs (rejected: not
diff-reviewable).

## Client config generation & delivery

- Per-user generator `xray-user-xrN` emits both `uuid` (secret) and
  `client.json` (secret), with `dependencies` on `xray-front-params`,
  `xray-reality-front`, and `xray-geo`. Identifying values are read at generation
  time via `$in/<dep>/<file>` — **not** interpolated as Nix string literals — so
  the generator script in git contains no identifying literals. Adding `xr3` =
  copy one generator block + add its UUID to the front inbound list.
- `client.json` contents:
  - VLESS outbound to the front: `address`/`port`/`serverName`/`publicKey`/
    `shortId` from vars; `flow: xtls-rprx-vision`; `fingerprint: chrome`;
    `spiderX`.
  - Split-tunnel router: `bypass-geosite` + `bypass-geoip` + private/LAN +
    localhost → `direct`; everything else → `proxy` (the front outbound).
  - Modest `dns` block: proxied DNS (via tunnel) for tunneled traffic, direct DNS
    for bypassed RU domains, to avoid leaks. DNS servers are generic public
    resolvers (not identifying).
- Share helper: `nix run .#xray-share -- xr1` decrypts that user's `client.json`
  (via clan vars), prints a `vless://…` import URL + a terminal QR (`qrencode`).
  Nothing stored; generated on demand. Implemented as a perSystem package/app.

## Provisioning & handoff

Assistant creates config + commits + `git add`s new files. Operator runs:

1. **`xray-exit` (Proxmox VM):** assistant creates machine module +
   `my-machines/xray-exit/disko.nix` + guest-VLAN net override + hostId +
   guest-VLAN IP. Operator: `nix run .#proxmox-vm-create -- xray-exit …` → run
   the `qm` commands → install → `clan machines update xray-exit`. Manual: router
   port-forward public `:PORT` → `192.168.3.x:8443`. (DDNS already exists.)
2. **`xray-front` (EC2):** assistant creates machine module + disko +
   amazon-image profile. Operator: launch a fresh instance + public address
   (Elastic IP or DNS — operator's choice) + security group (443 + SSH) →
   `nixos-anywhere` → `clan machines update xray-front`; set
   `xray-front-params/endpoint` to the public address.
3. **Secrets:** assistant may `clan vars set` camouflage/geo defaults; operator
   runs `clan vars generate` (random keys/UUIDs) + deploy.
4. **Distribute** client links via `nix run .#xray-share -- <user>`.

## Testing / verification

- Build-time (assistant): `ncf eval all`;
  `nix build .#nixosConfigurations.xray-{front,exit}.config.system.build.toplevel`;
  `nix fmt && just lint`.
- Config validity: `xray -test` on the rendered configs.
- Live (operator, since assistant hands off): xray units active; end-to-end
  `curl ifconfig.me` through a client shows the **home residential IP**; a RU-geo
  site exits **direct**; REALITY handshake succeeds (probing the front just
  serves the camouflage site).

Honest limitation: the assistant can build/eval/lint but cannot run the live
end-to-end test without the deployed boxes.

## Files (anticipated)

- `modules/machines/xray-front.nix`
- `modules/machines/xray-exit.nix`
- `my-machines/xray-front/disko.nix`
- `my-machines/xray-exit/disko.nix`
- clan var generators (in the machine modules or a shared `modules/xray/*.nix`)
- `nix run .#xray-share` package (perSystem)
- `inventory/host-id.toml` (+2 lines), `inventory/networks/guest.toml` (+1 line)

## Open items for the implementation plan

- Exact camouflage `dest`/`sni` values (validate REALITY suitability).
- Whether generators live inline in machine modules or a shared `modules/xray/`
  tree (lean shared, since front-params/geo/link are shared).
- disko layouts (EC2 root vs Proxmox VM disk).
- Guest-VLAN IP allocation for `xray-exit`.
