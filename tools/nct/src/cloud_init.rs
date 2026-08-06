//! Cloud-init snippet builders for NoCloud (Proxmox CloudInit drive).
//!
//! Two files, both uploaded as Proxmox snippets and wired via
//! `--cicustom user=<s>:snippets/<name>,network=<s>:snippets/<name>`:
//!
//! - **user-data**: a `#cloud-config` doc with hostname, authorized SSH keys,
//!   and the machine's age private key under a dedicated `clan-machine-key:`
//!   top-level key. The whole doc is valid YAML (cloud-init ignores unknown
//!   top-level keys); `provision-clan-key.nix`'s initrd unit greps
//!   `^clan-machine-key:` to extract it.
//! - **network-config**: Network Config v2 (netplan-style) from the chosen
//!   inventory network. The values are *also* mirrored into Proxmox-native
//!   `--ipconfig0` / `--nameserver` (see `provision_pve.rs`) so `qm config`
//!   stays self-documenting; `cicustom network` is what the guest consumes.

use serde::{Deserialize, Serialize};

use crate::config::{IpAlloc, Network};

/// Build the `user-data` cloud-config string.
pub fn user_data(
    hostname: &str,
    authorized_keys: &[String],
    age_key: &str,
) -> String {
    // Hand-rolled YAML to keep the structure predictable and the
    // clan-machine-key line a clean top-level scalar (no quoting surprises).
    let mut out = String::from("#cloud-config\n");
    out.push_str(&format!("hostname: {hostname}\n"));
    out.push_str("ssh_authorized_keys:\n");
    if authorized_keys.is_empty() {
        out.push_str("  []\n");
    } else {
        for k in authorized_keys {
            out.push_str(&format!("  - {k}\n"));
        }
    }
    out.push_str(&format!("clan-machine-key: {age_key}\n"));
    // Allow cloud-init to apply the network-config on every boot, not just
    // boot-new-instance. NoCloud's default only allows network updates on a
    // new instance-id; without this, cloud-init skips network rendering on
    // first boot when the instance-id is stable (Proxmox's meta-data has a
    // fixed id), leaving the image's DHCP fallback in charge.
    out.push_str("updates:\n");
    out.push_str("  network:\n");
    out.push_str("    when: [boot]\n");
    out
}

/// Network Config v2 (netplan-style) from the inventory network + allocation.
#[derive(Debug, Serialize, Deserialize)]
struct NetplanV2 {
    version: u8,
    ethernets: Ethernets,
}

#[derive(Debug, Serialize, Deserialize)]
struct Ethernets {
    #[serde(rename = "eth0")]
    eth0: Eth,
}

#[derive(Debug, Serialize, Deserialize)]
struct Eth {
    addresses: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    routes: Option<Vec<Route>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    nameservers: Option<Nameservers>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Route {
    to: String,
    via: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Nameservers {
    addresses: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    search: Option<Vec<String>>,
}

/// Build the `network-config` v2 string.
///
/// VLAN is *not* set here: it's tagged at the Proxmox NIC level
/// (`--net0 ...tag=<vlan>`) so the guest sees untagged eth0.
pub fn network_config(ip: &IpAlloc, net: &Network) -> String {
    let addresses = vec![format!("{}/{}", ip.address, net.prefix)];
    let routes = net.gateway.as_ref().map(|gw| {
        vec![Route {
            to: "default".into(),
            via: gw.clone(),
        }]
    });
    let nameservers = if net.dns.is_empty() {
        None
    } else {
        Some(Nameservers {
            addresses: net.dns.clone(),
            search: net.searchdomain.as_ref().map(|s| vec![s.clone()]),
        })
    };
    let plan = NetplanV2 {
        version: 2,
        ethernets: Ethernets {
            eth0: Eth {
                addresses,
                routes,
                nameservers,
            },
        },
    };
    serde_yaml::to_string(&plan).expect("netplan v2 is always serializable")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn user_data_contains_targeted_key() {
        let ud = user_data("xray-exit", &["ssh-ed25519 AAA".into()], "AGE-SECRET-KEY-1XYZ");
        assert!(ud.starts_with("#cloud-config\n"));
        assert!(ud.contains("hostname: xray-exit\n"));
        assert!(ud.contains("  - ssh-ed25519 AAA\n"));
        assert!(ud.contains("clan-machine-key: AGE-SECRET-KEY-1XYZ\n"));
        // And the grep target the initrd unit uses:
        assert!(ud.lines().any(|l| l.starts_with("clan-machine-key:")));
        // And the network-update gate opener:
        assert!(ud.contains("updates:\n"));
        assert!(ud.contains("  network:\n"));
        assert!(ud.contains("    when: [boot]\n"));
    }

    #[test]
    fn user_data_empty_keys_list() {
        let ud = user_data("h", &[], "AGE-SECRET-KEY-1X");
        assert!(ud.contains("ssh_authorized_keys:\n  []\n"));
    }

    #[test]
    fn network_config_has_address_route_dns() {
        let ip = IpAlloc {
            address: "192.168.3.10".into(),
            mac: None,
        };
        let net = Network {
            prefix: 24,
            gateway: Some("192.168.3.1".into()),
            dns: vec!["192.168.3.1".into()],
            vlan: Some(4),
            searchdomain: Some("guest.binarin.info".into()),
        };
        let nc = network_config(&ip, &net);
        assert!(nc.contains("version: 2"));
        assert!(nc.contains("addresses:"));
        assert!(nc.contains("- 192.168.3.10/24"));
        assert!(nc.contains("to: default"));
        assert!(nc.contains("via: 192.168.3.1"));
        assert!(nc.contains("- 192.168.3.1"));
        assert!(nc.contains("- guest.binarin.info"));
        // VLAN deliberately absent here:
        assert!(!nc.contains("vlan"));
    }
}
