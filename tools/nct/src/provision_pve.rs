//! `nct machine provision-pve`: provision a cloud-image-based Proxmox VM with
//! the clan age key injected via cloud-init.
//!
//! Flow (mirrors ncf's `provision_vm.run()`, adapted for cloud images):
//!  1. Load config (one nix eval, cached).
//!  2. Decrypt the machine age key via `clan secrets get`.
//!  3. Check VM existence by hostname; bail / validate if present.
//!  4. Build the cloud image (`system.build.cloudImage` -> qcow2).
//!  5. Generate + upload cloud-init snippets (user-data + network-config).
//!  6. `qm create` with cicustom + ipconfig0 (in sync) + NIC/vlan.
//!  7. EFI disk (if ovmf) + TPM2 (if enabled).
//!  8. Import qcow2 via `qm set --scsi0 <storage>:0,import-from=...`.
//!  9. Set boot order; optionally start.

use std::path::PathBuf;

use anyhow::{Context, Result, bail};

use crate::cloud_init;
use crate::config::{self, VmConfig};
use crate::image_inject;
use crate::nix_flake::NixFlake;
use crate::proxmox::Proxmox;

pub struct ProvisionPveArgs {
    pub machine: String,
    pub proxmox_host: String,
    pub network: String,
    pub bridge: String,
    pub snippet_storage: String,
    pub disk_storage: Option<String>,
    pub start: bool,
    /// Skip qcow2 build+rsync if the remote image already exists, and don't
    /// delete it afterwards. For fast iteration without rebuilding the image.
    pub test_reuse_image: bool,
    /// Inject the clan age key directly into the qcow2 (via guestfish) rather
    /// than shipping it via the cloud-init seed. Implies rebuilding the image
    /// each run (--test-reuse-image is refused, since a reused image may
    /// carry a stale key).
    pub inject_key: bool,
    pub dry_run: bool,
}

pub async fn run(flake: &NixFlake, args: ProvisionPveArgs) -> Result<()> {
    let ProvisionPveArgs {
        machine,
        proxmox_host,
        network,
        bridge,
        snippet_storage,
        disk_storage,
        start,
        test_reuse_image,
        inject_key,

        dry_run,
    } = args;

    // Injecting the key bakes a secret into the image, so a reused (stale)
    // image may carry a wrong key. Refuse the combination up front.
    if inject_key && test_reuse_image && !dry_run {
        bail!(
            "--inject-key injects the key into a freshly built image; it is \
             incompatible with --test-reuse-image (which would skip the build \
             and reuse a possibly stale key). Drop one of the flags."
        );
    }

    println!("Provisioning VM: {machine} on {proxmox_host}");

    // 1. Config (single eval).
    println!("\nStep 1: gathering metadata from NixOS config (network: {network})");
    let cfg = config::load_machine_config(flake, &machine, &network)
        .context("loading machine config via nix-bindings")?;
    let vmc = cfg.proxmox();
    println!("  hostname: {}", cfg.hostname());
    println!("  memory/cores: {} MB / {}", vmc.memory, vmc.cores);
    println!("  bios: {}", vmc.bios);
    println!(
        "  ip: {} (vlan {:?})",
        cfg.ip_alloc().address,
        cfg.net().vlan
    );

    // 2. Age key.
    println!("\nStep 2: decrypting clan age key");
    let age_key = if dry_run {
        "AGE-SECRET-KEY-1DRYRUNPLACEHOLDER".to_string()
    } else {
        clan_secret_get(&machine).with_context(|| format!("getting {machine}-age.key"))?
    };
    println!("  ok ({} bytes)", age_key.len());

    // 3. VM existence + allocate vmid (needed early so we can name the
    //    remote image with it).
    let pve = Proxmox::new(&proxmox_host);
    if !dry_run && let Some(existing) = pve.vmid_for_name(cfg.hostname()).await? {
        bail!(
            "VM '{}' already exists (vmid {existing}). Delete it first: \
             ssh root@{} 'qm stop {} && qm destroy {} --purge'",
            cfg.hostname(),
            proxmox_host,
            existing,
            existing
        );
    }
    let vmid = if dry_run { 999 } else { pve.next_vmid().await? };

    // 4. Build cloud image (skipped if --test-reuse-image and the remote
    //    image already exists). Named with vmid so concurrent/iterative runs
    //    don't collide.
    let remote_image = format!("/tmp/{machine}-{vmid}-disk.qcow2");
    let skip_build = test_reuse_image && !dry_run && pve.file_exists(&remote_image).await?;
    let image_path = if dry_run {
        println!("\nStep 4: (dry-run) cloud image");
        PathBuf::from("/tmp/dry-run.qcow2")
    } else if skip_build {
        println!("\nStep 4: reusing remote image {remote_image} (--test-reuse-image)");
        PathBuf::from(&remote_image)
    } else {
        println!("\nStep 4: building cloud image");
        let p = build_cloud_image(&machine)?;
        println!("  image: {}", p.display());
        p
    };

    // 4b. Inject the clan age key directly into the qcow2 (guestfish), so the
    //     cloud-init seed no longer needs to carry it. The destination is the
    //     resolved secretLocation/key.txt from the machine config — same path
    //     provision-clan-key would write, so clan's Stage 2 decryption finds
    //     it without changes.
    let image_path = if inject_key && !dry_run && !skip_build {
        println!("\nStep 4b: injecting age key into image");
        let dest = cfg.secret_key_path();
        println!("  target: {dest}");
        let injected = image_inject::inject_age_key(&image_path, &dest, &age_key)
            .context("injecting age key into image")?;
        println!("  injected -> {}", injected.display());
        injected
    } else if inject_key && dry_run {
        println!("\nStep 4b: (dry-run) would inject age key into image");
        println!(
            "  guestfish --rw -a {} : run : list-filesystems (pick ext4 root)",
            image_path.display(),
        );
        println!(
            "           : mount <ext4> / : mkdir-p {} : upload <tmp> {} : chmod 0600 {}",
            image_inject::parent_dir(&cfg.secret_key_path()),
            cfg.secret_key_path(),
            cfg.secret_key_path(),
        );
        image_path
    } else {
        image_path
    };

    // 5. Cloud-init snippets.
    let userdata = cloud_init::user_data(cfg.hostname(), cfg.authorized_keys(), &age_key);
    let netcfg = cloud_init::network_config(cfg.ip_alloc(), cfg.net());
    let user_snip = format!("{}-ci-user.yaml", cfg.hostname());
    let net_snip = format!("{}-ci-network.yaml", cfg.hostname());
    if dry_run {
        println!("\nStep 5 (dry-run): would upload snippets:");
        println!("--- {user_snip} ---\n{userdata}--- {net_snip} ---\n{netcfg}");
    } else {
        println!("\nStep 5: uploading cloud-init snippets to {snippet_storage}");
        let user_dir = pve.snippet_path(&snippet_storage, &user_snip).await?;
        let net_dir = pve.snippet_path(&snippet_storage, &net_snip).await?;
        pve.write_file(&user_dir, &userdata).await?;
        pve.write_file(&net_dir, &netcfg).await?;
        println!("  uploaded {user_snip} + {net_snip}");
    }

    // 6. qm create.
    println!("\nStep 6: creating VM {vmid}");
    let disk_storage = disk_storage
        .clone()
        .or_else(|| first_disk_storage(vmc))
        .unwrap_or_else(|| "local-zfs".into());
    let create_args = build_qm_create(
        vmid,
        cfg.hostname(),
        vmc,
        cfg.ip_alloc(),
        cfg.net(),
        &bridge,
        &disk_storage,
        &snippet_storage,
        &user_snip,
        &net_snip,
    );
    if dry_run {
        println!("  would run: qm {}", shell_words::join(&create_args));
    } else {
        let args_ref: Vec<&str> = create_args.iter().map(|s| s.as_str()).collect();
        pve.qm(&args_ref).await?;
        println!("  created");
    }

    // 7. EFI + TPM.
    if vmc.bios == "ovmf" {
        println!("\nStep 7a: configuring EFI disk");
        let efi = &vmc.efidisk;
        let storage = efi.storage.clone().unwrap_or_else(|| disk_storage.clone());
        let efitype = efi.efitype.clone().unwrap_or_else(|| "4m".into());
        let pre_enrolled = if efi.secure_boot { "1" } else { "0" };
        let spec = format!("{storage}:1,efitype={efitype},pre-enrolled-keys={pre_enrolled}");
        if dry_run {
            println!("  would run: qm set {vmid} --efidisk0 {spec}");
        } else {
            pve.qm(&["set", &vmid.to_string(), "--efidisk0", &spec])
                .await?;
        }
    }
    if vmc.tpm2.enable {
        println!("\nStep 7b: configuring TPM2");
        let storage = vmc
            .tpm2
            .storage
            .clone()
            .unwrap_or_else(|| disk_storage.clone());
        let version = vmc.tpm2.version.clone().unwrap_or_else(|| "v2.0".into());
        let spec = format!("{storage}:1,version={version}");
        if dry_run {
            println!("  would run: qm set {vmid} --tpmstate0 {spec}");
        } else {
            pve.qm(&["set", &vmid.to_string(), "--tpmstate0", &spec])
                .await?;
        }
    }

    // 8. Import qcow2 (skip rsync if we reused the remote image).
    println!("\nStep 8: importing disk image");
    // Resize the imported image to the configured size (if any). The qcow2 is
    // built at the image's native (small) size; `qm resize` grows the virtual
    // disk, then cloud-init's growpart+resizefs expand the partition + fs.
    let disk_size = vmc.disks.first().and_then(|d| d.size.clone());
    if dry_run {
        println!(
            "  would rsync {} -> root@{}:{remote_image}",
            image_path.display(),
            proxmox_host
        );
        println!("  would run: qm set {vmid} --scsi0 {disk_storage}:0,import-from={remote_image}");
        if let Some(ref sz) = disk_size {
            println!("  would run: qm resize {vmid} scsi0 {sz}");
        }
    } else {
        if !skip_build {
            pve.rsync_to(&image_path, &remote_image).await?;
        } else {
            println!("  (reusing {remote_image}, skipping rsync)");
        }
        let spec = format!("{disk_storage}:0,import-from={remote_image}");
        pve.qm(&["set", &vmid.to_string(), "--scsi0", &spec])
            .await?;
        pve.qm(&["set", &vmid.to_string(), "--boot", "order=scsi0"])
            .await?;
        if let Some(ref sz) = disk_size {
            println!("  resizing scsi0 to {sz}");
            pve.qm(&["resize", &vmid.to_string(), "scsi0", sz]).await?;
        }
        // Cleanup the temp image on the host — unless we're keeping it for reuse.
        if !test_reuse_image {
            pve.run_remote(&[format!("rm -f {remote_image}")])
                .await
                .ok();
        }
    }

    // 9. Start.
    if start {
        println!("\nStep 9: starting VM");
        if dry_run {
            println!("  would run: qm start {vmid}");
        } else {
            pve.qm(&["start", &vmid.to_string()]).await?;
        }
    }

    println!("\nDone! vmid={vmid}");
    Ok(())
}

/// Run `nix build` for `system.build.cloudImage` and return the qcow2 path.
fn build_cloud_image(machine: &str) -> Result<PathBuf> {
    let attr = format!(".#nixosConfigurations.{machine}.config.system.build.cloudImage");
    println!("  nix build {attr}");
    let out = std::process::Command::new("nix")
        .args(["build", "--no-link", "--print-out-paths", &attr])
        .output()
        .context("spawning nix build")?;
    if !out.status.success() {
        bail!(
            "nix build failed: {}\n{}",
            out.status,
            String::from_utf8_lossy(&out.stderr).trim()
        );
    }
    let dir = String::from_utf8_lossy(&out.stdout).trim().to_owned();
    Ok(PathBuf::from(dir).join("nixos.qcow2"))
}

/// Decrypt `<machine>-age.key` via clan.
fn clan_secret_get(machine: &str) -> Result<String> {
    let name = format!("{machine}-age.key");
    // Inherit stdin + stderr so interactive prompts (yubikey PIN touch/pinentry)
    // work on a real terminal, but capture stdout since we need the key value.
    let out = std::process::Command::new("clan")
        .args(["secrets", "get", &name])
        .stdin(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .output()
        .with_context(|| format!("spawning `clan secrets get {name}` (is clan on PATH?)"))?;
    if !out.status.success() {
        bail!("clan secrets get {name} failed: {}", out.status);
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_owned())
}

fn first_disk_storage(vmc: &VmConfig) -> Option<String> {
    vmc.disks.first().and_then(|d| d.storage.clone())
}

/// Build the `qm create` argv (as owned Strings, for shell_words::join).
#[allow(clippy::too_many_arguments)]
fn build_qm_create(
    vmid: u64,
    hostname: &str,
    vmc: &VmConfig,
    ip: &config::IpAlloc,
    net: &config::Network,
    bridge: &str,
    _disk_storage: &str,
    snippet_storage: &str,
    user_snip: &str,
    net_snip: &str,
) -> Vec<String> {
    let mut cmd: Vec<String> = vec![
        "create".into(),
        vmid.to_string(),
        "--name".into(),
        hostname.into(),
        "--memory".into(),
        vmc.memory.to_string(),
        "--cores".into(),
        vmc.cores.to_string(),
        "--sockets".into(),
        vmc.sockets.to_string(),
        "--bios".into(),
        vmc.bios.clone(),
        "--machine".into(),
        vmc.machine.clone(),
        "--scsihw".into(),
        vmc.scsihw.clone(),
    ];
    if vmc.onboot {
        cmd.push("--onboot".into());
        cmd.push("1".into());
    }
    if vmc.agent {
        cmd.push("--agent".into());
        cmd.push("1".into());
    }
    if let Some(b) = vmc.balloon {
        cmd.push("--balloon".into());
        cmd.push(b.to_string());
    }
    if let Some(s) = vmc.shares
        && s != 1000
    {
        cmd.push("--shares".into());
        cmd.push(s.to_string());
    }
    if let Some(desc) = &vmc.description {
        cmd.push("--description".into());
        cmd.push(desc.clone());
    }

    // NIC: bridge + model + firewall + mac + vlan tag.
    let mut net_spec = format!("virtio,bridge={bridge},firewall=1");
    if let Some(mac) = &ip.mac {
        net_spec.push_str(&format!(",macaddr={mac}"));
    }
    if let Some(vlan) = net.vlan {
        net_spec.push_str(&format!(",tag={vlan}"));
    }
    cmd.push("--net0".into());
    cmd.push(net_spec);

    cmd.push("--serial0".into());
    cmd.push("socket".into());
    cmd.push("--vga".into());
    cmd.push("serial0".into());

    // cloud-init snippets: user (age key + authorized_keys) + network (v2).
    cmd.push("--cicustom".into());
    cmd.push(format!(
        "user={snippet_storage}:snippets/{user_snip},network={snippet_storage}:snippets/{net_snip}"
    ));

    // Mirror networking into Proxmox-native ipconfig0 / nameserver so qm config
    // is self-documenting (cicustom network is what the guest actually uses).
    if let Some(gw) = &net.gateway {
        cmd.push("--ipconfig0".into());
        cmd.push(format!("ip={}/{},gw={}", ip.address, net.prefix, gw));
    }
    if !net.dns.is_empty() {
        cmd.push("--nameserver".into());
        cmd.push(net.dns.join(","));
    }

    // cloud-init drive (the NoCloud seed source).
    cmd.push("--ide2".into());
    cmd.push(format!("{}:cloudinit", vmc.cloud_init.storage));

    cmd
}
