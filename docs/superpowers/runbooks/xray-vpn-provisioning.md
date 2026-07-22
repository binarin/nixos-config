# xray VPN — provisioning runbook (operator)

Prereqs: build/eval green (`nix flake check`-level evals pass; see plan tasks 4–6).
The assistant writes/stages config; you run everything below.

## 1. Generate secrets
Allocate the EC2 Elastic IP (or DNS) first so you can answer the front endpoint prompt.

```bash
# Shared + per-user vars. You'll be prompted for:
#   xray-front-params/endpoint = <EC2 Elastic IP or DNS>
#   xray-exit-params/endpoint  = <your existing home DDNS host>
clan vars generate xray-exit
clan vars generate xray-front
```
Optional overrides (camouflage / geo) later, e.g.:
```bash
clan vars set xray-front xray-front-params/sni    aws.amazon.com
clan vars set xray-front xray-front-params/dest   aws.amazon.com:443
clan vars set xray-front xray-geo/bypass-geosite  geosite:category-ru
```

## 2. Home exit VM (Proxmox, guest VLAN 192.168.3.10)
```bash
nix run .#proxmox-vm-create -- xray-exit --dry-run   # review qm commands
nix run .#proxmox-vm-create -- xray-exit             # create the VM
# install NixOS onto it (disko + toplevel), then:
clan machines update xray-exit
```
Router: forward public TCP `:443` → `192.168.3.10:8443`. DDNS host must resolve to your home IP.

## 3. AWS front VM (EC2, nixos-anywhere)
- Launch a fresh instance (UEFI boot mode; Nitro type so the disk is `/dev/nvme0n1` — otherwise edit `my-machines/xray-front/disko.nix`).
- Security group: inbound TCP 22 (your IP) + 443 (world).
- Attach the Elastic IP you entered as `xray-front-params/endpoint`.

```bash
nix run github:nix-community/nixos-anywhere -- \
  --flake .#xray-front --target-host root@<ec2-dns>
clan machines update xray-front --target-host root@<ec2-dns>
```

## 4. Distribute client configs
```bash
nix run .#xray-share -- xr1   # prints vless:// URL + QR
nix run .#xray-share -- xr2
```

## 5. Verify (live)
- `systemctl status xray` active on both boxes; `journalctl -u xray` clean.
- From a client (import the vless URL): `curl https://ifconfig.me` → your **home residential IP**.
- A RU site (e.g. `curl -s https://api.myip.com` while visiting a `geosite:category-ru` domain) exits **direct**.
- Probing the front (`curl -skI https://<ec2-dns>`) serves the camouflage site, not an error.

## Adding a user later
1. Add the name to `userNames` in `modules/xray/config-lib.nix`.
2. `clan vars generate xray-front` (creates the new UUID + client.json; updates the front inbound).
3. `clan machines update xray-front` and `nix run .#xray-share -- <name>`.
