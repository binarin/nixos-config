//! `nct machine build-aws-image <name>` — build the EC2 AMI artifact
//! (`system.build.amazonImage`, a VHD) and optionally inject the clan age key.
//!
//! This is the pure-ish, AWS-credential-free half of the EC2 provision flow.
//! `provision-aws` (opentofu) consumes the VHD this produces and does the
//! credential-bearing S3/import/instance dance.
//!
//! Flow:
//!   1. `nix build .#nixosConfigurations.<name>.config.system.build.amazonImage`.
//!   2. Read `<out>/nix-support/image-info.json` for the VHD path + metadata.
//!   3. If `--inject-key`: decrypt the machine age key via `clan secrets get`
//!      and inject it into the VHD (guestfish, same mechanism as provision-pve).
//!      The injected VHD is written to a stable tempfile and printed; the
//!      caller (or the operator) uploads it to S3 for AMI import.
//!
//! The base VHD lives in the read-only nix store, so the injected copy is
//! materialised in `$TMPDIR`. Without `--inject-key` we just print the store
//! path — the operator can upload it directly.

use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use serde::Deserialize;

use crate::image_inject;

/// Metadata from `image-info.json` (emitted by nixpkgs' amazon-image builder).
#[derive(Debug, Deserialize)]
struct ImageInfo {
    /// Store path of the VHD file (absolute).
    file: String,
    /// Virtual size in bytes (string-encoded; nixpkgs emits it via jq as a
    /// string to preserve the exact integer).
    #[serde(rename = "logical_bytes")]
    logical_bytes: String,
    /// Boot mode: "uefi" or "legacy-bios".
    boot_mode: String,
    /// NixOS version label.
    label: String,
    /// System string, e.g. "x86_64-linux".
    system: String,
}

/// Run `nix build` for `system.build.amazonImage` and return the out dir.
fn build_amazon_image(machine: &str) -> Result<PathBuf> {
    let attr = format!(".#nixosConfigurations.{machine}.config.system.build.amazonImage");
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
    Ok(PathBuf::from(dir))
}

/// Read `<out>/nix-support/image-info.json`.
fn read_image_info(out_dir: &PathBuf) -> Result<ImageInfo> {
    let path = out_dir.join("nix-support").join("image-info.json");
    let raw =
        std::fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?;
    let info: ImageInfo = serde_json::from_str(&raw).context("parsing image-info.json")?;
    Ok(info)
}

/// Decrypt `<machine>-age.key` via clan (mirrors provision_pve::clan_secret_get).
fn clan_secret_get(machine: &str) -> Result<String> {
    let name = format!("{machine}-age.key");
    // Inherit stdin + stderr so interactive prompts (yubikey PIN / pinentry)
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

pub struct BuildAwsImageArgs {
    pub machine: String,
    /// Inject the clan age key into the VHD before printing its path.
    pub inject_key: bool,
    /// Destination path inside the image for the age key. Defaults to the
    /// standard sops-nix location; provision-pve resolves this from config,
    /// but for EC2 (where we don't load the full MachineConfig here) the
    /// operator can override. clan's sops backend reads
    /// /var/lib/sops-nix/key.txt by default.
    pub key_dest: String,
}

pub fn run(args: BuildAwsImageArgs) -> Result<()> {
    // 1. Build the amazonImage VHD.
    let out_dir = build_amazon_image(&args.machine)?;
    let info = read_image_info(&out_dir)?;
    println!(
        "  built: {} ({} {}, {} bytes, boot={})",
        info.system, info.label, info.boot_mode, info.logical_bytes, info.file
    );

    // 2. Optionally inject the age key.
    let vhd_to_upload: PathBuf = if args.inject_key {
        println!("  decrypting clan age key for {}...", args.machine);
        let key = clan_secret_get(&args.machine)?;
        println!("  injecting age key -> {} (via guestfish)", args.key_dest);
        let injected =
            image_inject::inject_age_key(std::path::Path::new(&info.file), &args.key_dest, &key)
                .context("injecting age key into VHD")?;
        println!("  injected VHD: {}", injected.display());
        injected
    } else {
        // No injection: upload the store VHD directly.
        PathBuf::from(&info.file)
    };

    // 3. Print the final VHD path (the artifact to upload to S3 for AMI import).
    println!();
    println!("VHD: {}", vhd_to_upload.display());
    if !args.inject_key {
        println!("(store path — upload directly, or re-run with --inject-key)");
    }
    Ok(())
}
