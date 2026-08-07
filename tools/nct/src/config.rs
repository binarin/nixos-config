//! Typed view of the NixOS config attrs `provision-pve` needs, pulled from a
//! single forced `nixosConfigurations.<name>` value via the long-lived
//! [`crate::nix_flake::NixFlake`] worker.
//!
//! Extraction strategy: rather than marshalling the whole (huge) qemu-guest
//! schema through serde, we navigate via a single Nix lambda that returns
//! `builtins.toJSON` of exactly the attrs we need. The module eval is forced
//! once (inside that one `call`); all the sub-reads are thunk hits within it.
//!
//! `secretLocation` / `secretKeyPath` mirror the resolution logic in
//! `modules/provision-clan-key.nix`: the clan age key must land where clan's
//! Stage 2 decryption reads it, which depends on the configured secret store
//! (`sops` -> `clan.core.vars.sops.secretUploadDirectory`, else ->
//! `clan.core.vars.age.secretLocation`).

use serde::{Deserialize, Serialize};

use crate::nix_flake::NixFlake;

/// The `config.nixos-config.qemu-guest.proxmox` block.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VmConfig {
    pub memory: u64,
    #[serde(default)]
    pub balloon: Option<u64>,
    pub cores: u64,
    #[serde(default = "default_one")]
    pub sockets: u64,
    #[serde(default = "default_seabios")]
    pub bios: String,
    #[serde(default = "default_q35")]
    pub machine: String,
    #[serde(default = "default_scsihw")]
    pub scsihw: String,
    #[serde(default)]
    pub onboot: bool,
    #[serde(default = "default_true")]
    pub agent: bool,
    #[serde(default)]
    pub shares: Option<u64>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub disks: Vec<Disk>,
    #[serde(default, rename = "cloudInit")]
    pub cloud_init: CloudInit,
    #[serde(default)]
    pub tpm2: Tpm2,
    #[serde(default)]
    pub efidisk: Efidisk,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Disk {
    #[serde(rename = "type")]
    pub kind: String,
    #[serde(default)]
    pub storage: Option<String>,
    #[serde(default)]
    pub size: Option<String>,
    #[serde(default)]
    pub bus: Option<String>,
    #[serde(default)]
    pub index: Option<u64>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct CloudInit {
    #[serde(default = "default_true")]
    pub enable: bool,
    #[serde(default = "default_local_zfs")]
    pub storage: String,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct Tpm2 {
    #[serde(default)]
    pub enable: bool,
    #[serde(default)]
    pub storage: Option<String>,
    #[serde(default)]
    pub version: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct Efidisk {
    #[serde(default)]
    pub storage: Option<String>,
    #[serde(default)]
    pub efitype: Option<String>,
    #[serde(rename = "secureBoot", default)]
    pub secure_boot: bool,
}

/// `config.inventory.hostIpAllocation.<net>.primary`.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct IpAlloc {
    pub address: String,
    #[serde(default)]
    pub mac: Option<String>,
}

/// `config.inventory.networks.<net>`.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct Network {
    pub prefix: u8,
    #[serde(default)]
    pub gateway: Option<String>,
    #[serde(default)]
    pub dns: Vec<String>,
    #[serde(default)]
    pub vlan: Option<u32>,
    #[serde(default)]
    pub searchdomain: Option<String>,
}

fn default_one() -> u64 {
    1
}
fn default_true() -> bool {
    true
}
fn default_seabios() -> String {
    "seabios".into()
}
fn default_q35() -> String {
    "q35".into()
}
fn default_scsihw() -> String {
    "virtio-scsi-single".into()
}
fn default_local_zfs() -> String {
    "local-zfs".into()
}

/// The bundle of config + the raw JSON (for diagnostics / dry-run dumps).
pub struct MachineConfig {
    inner: MachineConfigInner,
    pub json: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct MachineConfigInner {
    #[serde(rename = "hostName")]
    hostname: String,
    #[serde(rename = "hostId")]
    host_id: String,
    #[serde(rename = "ipAlloc")]
    ip_alloc: IpAlloc,
    #[serde(rename = "authorizedKeys")]
    authorized_keys: Vec<String>,
    proxmox: VmConfig,
    net: Network,
    #[serde(rename = "secretLocation")]
    secret_location: String,
}

/// Load all config fragments `provision-pve` needs for `machine` on `network`,
/// in **one** eval (a single Nix lambda returning `builtins.toJSON` of an
/// attrset). The module eval is forced once inside that `call`; all sub-reads
/// are thunk hits within it.
pub fn load_machine_config(
    flake: &NixFlake,
    machine: &str,
    network: &str,
) -> anyhow::Result<MachineConfig> {
    // Resolve the clan secret location exactly as `provision-clan-key.nix`
    // does: sops store -> sops.secretUploadDirectory, else ->
    // age.secretLocation. The key file is always `<dir>/key.txt`. This must
    // stay in lockstep with the module so an injected key lands where clan's
    // Stage 2 decryption (sops-nix's age.keyFile wiring) actually reads it.
    let secret_location_expr = "let\n\
     \x20   vars = m.config.clan.core.vars;\n\
     \x20   store = vars.settings.secretStore or \"\";\n\
     \x20 in\n\
     \x20   if store == \"sops\" then vars.sops.secretUploadDirectory or \"/var/lib/sops-nix\"\n\
     \x20   else vars.age.secretLocation or \"/etc/secret-vars\"\n";
    let lambda = format!(
        "m: builtins.toJSON {{\n\
         \x20 hostName = m.config.networking.hostName;\n\
         \x20 hostId = m.config.networking.hostId;\n\
         \x20 proxmox = m.config.nixos-config.qemu-guest.proxmox;\n\
         \x20 ipAlloc = m.config.inventory.hostIpAllocation.{network}.primary;\n\
         \x20 net = m.config.inventory.networks.{network};\n\
         \x20 authorizedKeys = m.config.users.users.root.openssh.authorizedKeys.keys;\n\
         \x20 secretLocation = {secret_location_expr};\n\
         }}"
    );
    let path = format!("nixosConfigurations.{machine}");
    let json = flake.apply(&path, &lambda)?;
    let json = json.trim();
    Ok(MachineConfig {
        inner: serde_json::from_str(json)?,
        json: json.to_owned(),
    })
}

#[allow(dead_code)]
impl MachineConfig {
    pub fn hostname(&self) -> &str {
        &self.inner.hostname
    }
    pub fn host_id(&self) -> &str {
        &self.inner.host_id
    }
    /// vsock CID = hostId parsed as hex.
    pub fn vsock_cid(&self) -> anyhow::Result<u64> {
        Ok(u64::from_str_radix(
            self.inner.host_id.trim_start_matches("0x"),
            16,
        )?)
    }
    pub fn proxmox(&self) -> &VmConfig {
        &self.inner.proxmox
    }
    pub fn ip_alloc(&self) -> &IpAlloc {
        &self.inner.ip_alloc
    }
    pub fn net(&self) -> &Network {
        &self.inner.net
    }
    pub fn authorized_keys(&self) -> &[String] {
        &self.inner.authorized_keys
    }
    /// Resolved clan secret directory (sops store -> secretUploadDirectory,
    /// else age.secretLocation), e.g. `/var/lib/sops-nix`.
    pub fn secret_location(&self) -> &str {
        &self.inner.secret_location
    }
    /// Full path where the clan age key must live for clan's Stage 2
    /// decryption to find it: `<secretLocation>/key.txt`.
    pub fn secret_key_path(&self) -> String {
        format!("{}/key.txt", self.inner.secret_location)
    }
}
