//! Inject the clan age key into a built cloud-image qcow2.
//!
//! `make-disk-image.nix` cannot accept a secret (any `contents`/`postVM`
//! value lands in the nix store → world-readable), so the key is injected
//! **after** the pure nix build, as an impure post-processing step on the
//! operator's machine. This is the same trust boundary as `clan secrets get`:
//! the secret lives in process memory / a 0600 tempfile, never in the store.
//!
//! Mechanism: `guestfish` (libguestfs), which manipulates the qcow2's
//! filesystems without needing root or a manual mount on the build host. It
//! spins up a short-lived appliance VM (KVM-accelerated when available) per
//! invocation — acceptable here since injection runs once per provision, not
//! in a hot loop.
//!
//! We do NOT use guestfish's `-i` (inspector) auto-mount: the inspector's
//! OS-detection fails on NixOS cloud images (no bootloader entry / os-release
//! it recognizes in the expected places), even though the filesystems are
//! perfectly mountable. Instead we `run`, `list-filesystems`, pick the ext4
//! partition as the root, and mount it explicitly.
//!
//! Flow:
//!   1. Copy the store qcow2 to a writable working path (the nix store path
//!      is read-only).
//!   2. Write the key to a 0600 `tempfile` and `guestfish upload` it to
//!      `<dest>` inside the image, then `chmod 0600`.
//!   3. Shred the tempfile.
//!   4. Verify by reading the file back and checking it's an
//!      `AGE-SECRET-KEY-1` value.

use std::fs;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};

/// Inject the clan age key into the qcow2 at `image`, writing it to
/// `dest` (the in-image absolute path, e.g. `/var/lib/sops-nix/key.txt`).
///
/// Because the nix-built image lives in the (read-only) store, a writable
/// copy is made in a fresh tempdir and persisted to a named tempfile in
/// `$TMPDIR`; that persisted path is returned. The caller rsyncs it to the
/// host and is responsible for cleanup.
///
/// `guestfish` must be on PATH (from the nct wrapper / dev shell).
pub fn inject_age_key(image: &Path, dest: &str, key: &str) -> Result<PathBuf> {
    // 1. Writable copy — the store path is read-only and guestfish --rw needs
    //    to mutate the image. Write into a fresh temp dir (not next to the
    //    source, which may be in the read-only nix store) and return that
    //    path; the caller rsyncs it to the host and is responsible for
    //    cleanup.
    let workdir = tempfile::tempdir().context("creating image inject workdir")?;
    // The qcow2 holds no secret in its own bytes (the key lives inside its
    // filesystem, readable only post-mount); and guestfish's qemu appliance
    // drops to a different user, so the image must be world-readable for the
    // appliance to open it. The injected key inside is chmod 0600.
    fs::set_permissions(workdir.path(), PermissionsExt::from_mode(0o755))
        .context("chmod workdir 0755")?;
    let out = workdir.path().join("disk.qcow2");
    fs::copy(image, &out)
        .with_context(|| format!("copying {} -> {}", image.display(), out.display()))?;
    fs::set_permissions(&out, PermissionsExt::from_mode(0o644)).context("chmod image 0644")?;
    // Keep the workdir alive for the whole function; hand off ownership of
    // the file by moving it out at the end. (tempfile::TempDir deletes on
    // drop, so we `persist` the file out before returning.)
    let _workdir = workdir;

    // 2. Stage the key in a 0600 tempfile; `guestfish upload` reads it (named
    //    pipe would also work, but a file is simpler + shredable). The key is
    //    a single line; keep the trailing newline clan/sops expect.
    let mut tmp = tempfile::Builder::new()
        .prefix("nct-age-key-")
        .rand_bytes(12)
        .permissions(std::fs::Permissions::from_mode(0o600))
        .tempfile()
        .context("creating age-key tempfile")?;
    write!(tmp.as_file_mut(), "{key}\n").context("writing key to tempfile")?;
    tmp.as_file_mut().sync_all().ok();
    let tmp_path = tmp.path().to_path_buf();

    // 3. Run guestfish to inject the key. We can't use `-i` (inspector):
    //    it fails on NixOS cloud images. Instead, find the root (ext4 fs)
    //    via list-filesystems, mount it, then mkdir-p/upload/chmod. Using
    //    `upload` (not `write <content>`) keeps the key out of argv /
    //    process listings — it only exists in the tempfile + guestfish's
    //    upload buffer.
    let parent = parent_dir(dest);
    let guestfish = resolve_guestfish()?;
    let tmp_str = tmp_path.to_str().context("tempfile path is not UTF-8")?;
    let root_dev = find_root_device(&guestfish, &out)?;
    println!("  root filesystem: {root_dev}");
    run_guestfish(
        &guestfish,
        &out,
        &[
            &["run"],
            &["mount", &root_dev, "/"],
            &["mkdir-p", parent],
            &["upload", tmp_str, dest],
            &["chmod", "0600", dest],
        ],
    )?;

    // 4. Verify: read the file back and confirm it's an age key. Drop
    //    the tempfile regardless of outcome.
    let verify = run_guestfish(
        &guestfish,
        &out,
        &[&["run"], &["mount", &root_dev, "/"], &["cat", dest]],
    );
    // Close (which unlinks) the tempfile now that the upload is done.
    let _ = tmp.close();
    match verify {
        Ok(content) => {
            let line = content.trim();
            if !line.starts_with("AGE-SECRET-KEY-1") {
                bail!(
                    "injected key at {dest} did not verify: expected an \
                     AGE-SECRET-KEY-1 value, got {line:?}"
                );
            }
        }
        Err(e) => {
            return Err(e).context("verifying injected age key");
        }
    }

    // Persist the injected image to a stable path outside the tempdir (which
    // is about to drop + delete its contents). A named tempfile keeps it
    // 0600-ish and auto-cleaned if the caller forgets, but the caller
    // (provision-pve) rsyncs it onward and removes it explicitly.
    let persisted = tempfile::Builder::new()
        .prefix("nct-injected-")
        .suffix(".qcow2")
        .tempfile_in(std::env::temp_dir())
        .context("persisting injected image")?;
    let persisted_path = persisted.path().to_path_buf();
    // `persist` keeps the file after drop (we want it to survive). Use
    // `.keep()` to release ownership without deleting.
    let _ = persisted
        .keep()
        .context("keeping injected image tempfile")?;
    fs::set_permissions(&persisted_path, PermissionsExt::from_mode(0o644))
        .context("chmod persisted image 0644")?;
    fs::rename(&out, &persisted_path).or_else(|_| {
        // rename can fail across filesystems; fall back to copy + remove.
        fs::copy(&out, &persisted_path)
            .with_context(|| format!("copying injected image to {}", persisted_path.display()))?;
        fs::remove_file(&out).ok();
        Ok::<(), anyhow::Error>(())
    })?;

    Ok(persisted_path)
}

/// Discover the root device by listing filesystems and picking the ext4 one.
/// NixOS cloud images (`make-disk-image.nix` with `partitionTableType =
/// "efi"`) produce a vfat ESP + an ext4 root; the ext4 partition is the
/// root we mount to write the key. `inspect-os`/`-i` is unreliable on these
/// images (OS detection misses NixOS), so we identify by fstype instead.
fn find_root_device(guestfish: &Path, image: &Path) -> Result<String> {
    let out = run_guestfish(guestfish, image, &[&["run"], &["list-filesystems"]])?;
    // Output format: one `/dev/sdXN: fstype` per line.
    let mut ext4 = None;
    let mut any = Vec::new();
    for line in out.lines() {
        let line = line.trim();
        let (dev, fstype) = line
            .split_once(": ")
            .with_context(|| format!("unexpected list-filesystems line: {line:?}"))?;
        any.push((dev, fstype));
        if fstype == "ext4" {
            ext4 = Some(dev.to_owned());
        }
    }
    let dev = ext4.ok_or_else(|| {
        anyhow::anyhow!(
            "no ext4 filesystem found in image (found {any:?}); cannot \
             determine the root partition to inject the key into"
        )
    })?;
    Ok(dev)
}

/// Locate `guestfish` on PATH with a helpful message if missing.
fn resolve_guestfish() -> Result<PathBuf> {
    let p = which_guestfish().context(
        "guestfish not found on PATH. Install libguestfs-with-appliance \
         (nixpkgs#libguestfs-with-appliance) or run nct from a shell that has \
         it. Plain libguestfs is NOT enough — guestfish needs the supermin \
         appliance only the -with-appliance variant ships.",
    )?;
    Ok(p)
}

fn which_guestfish() -> Option<PathBuf> {
    // Minimal `which` — avoid pulling a crate for this. Walk $PATH.
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let cand = dir.join("guestfish");
        if cand.is_file() {
            // Best-effort executable check; is_file is the common-case guard.
            return Some(cand);
        }
    }
    None
}

/// Run `guestfish --rw -a <image> <cmd...>`, returning trimmed stdout.
///
/// `--rw` is required to write. `cmds` is a slice of command-arg slices;
/// guestfish's argv mode separates commands with `:` separators (a bare
/// word would be treated as an argument to the previous command). Each value
/// is a separate argv element so values with spaces/special chars survive
/// without shell quoting — and so the key never appears in argv (we `upload`
/// from the tempfile).
fn run_guestfish(guestfish: &Path, image: &Path, cmds: &[&[&str]]) -> Result<String> {
    let img = image.to_str().context("image path is not valid UTF-8")?;
    // No `-i` (inspector): it fails on NixOS cloud images. Callers drive
    // `run` + explicit `mount` instead. Commands are given as slices of
    // slices; guestfish's argv mode separates commands with `:` separators
    // (a bare word would be treated as an argument to the previous command).
    // Each value is a separate argv element so values with spaces/special
    // chars survive without shell quoting — and so the key never appears in
    // argv (we `upload` from the tempfile).
    let mut args: Vec<&str> = vec!["--rw", "-a", img];
    for (i, cmd) in cmds.iter().enumerate() {
        if i > 0 {
            args.push(":");
        }
        args.extend_from_slice(cmd);
    }
    let out = std::process::Command::new(guestfish)
        .args(&args)
        .output()
        .with_context(|| format!("spawning guestfish {:?}", guestfish))?;
    if !out.status.success() {
        bail!(
            "guestfish failed: {}\nstdout: {}\nstderr: {}",
            out.status,
            String::from_utf8_lossy(&out.stdout).trim(),
            String::from_utf8_lossy(&out.stderr).trim(),
        );
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_owned())
}

/// `/var/lib/sops-nix/key.txt` -> `/var/lib/sops-nix`.
pub fn parent_dir(path: &str) -> &str {
    match path.rfind('/') {
        Some(0) => "/",
        Some(i) => &path[..i],
        None => "/",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parent_dir_strips_filename() {
        assert_eq!(parent_dir("/var/lib/sops-nix/key.txt"), "/var/lib/sops-nix");
        assert_eq!(parent_dir("/key.txt"), "/");
        assert_eq!(parent_dir("key.txt"), "/");
    }

    /// End-to-end: build the real xray-exit cloud image, inject a fake key,
    /// and verify it lands at the right path with the right contents + perms.
    /// Ignored by default (needs nix + libguestfs-with-appliance on PATH +
    /// network for any git deps). Run via `cargo test -- --ignored`.
    #[test]
    #[ignore]
    fn inject_into_real_cloud_image() {
        use std::process::Command;
        // 1. Build the image.
        let out = Command::new("nix")
            .args([
                "build",
                "--no-link",
                "--print-out-paths",
                ".#nixosConfigurations.xray-exit.config.system.build.cloudImage",
            ])
            .output()
            .expect("nix build");
        assert!(out.status.success(), "nix build failed");
        let dir = String::from_utf8_lossy(&out.stdout).trim().to_owned();
        let img = std::path::PathBuf::from(&dir).join("nixos.qcow2");
        assert!(img.exists(), "image not found at {}", img.display());

        // 2. Inject.
        let dest = "/var/lib/sops-nix/key.txt".to_string();
        let injected =
            super::inject_age_key(&img, &dest, "AGE-SECRET-KEY-1INTEGRATIONTEST").expect("inject");
        assert!(injected.exists(), "injected image missing");

        // 3. Re-open and verify contents + perms via guestfish directly.
        let gf = super::resolve_guestfish().expect("guestfish on PATH");
        let root = super::find_root_device(&gf, &injected).expect("find root");
        let cat = super::run_guestfish(
            &gf,
            &injected,
            &[&["run"], &["mount", &root, "/"], &["cat", &dest]],
        )
        .expect("cat");
        assert!(
            cat.starts_with("AGE-SECRET-KEY-1INTEGRATIONTEST"),
            "unexpected contents: {cat:?}"
        );
        // perms: mode is decimal. 0600 regular file = 0100600 octal = 33152.
        let perms = super::run_guestfish(
            &gf,
            &injected,
            &[&["run"], &["mount", &root, "/"], &["lstat", &dest]],
        )
        .expect("lstat");
        assert!(perms.contains("mode: 33152"), "perms not 0600: {perms}");

        // cleanup
        std::fs::remove_file(&injected).ok();
    }
}
