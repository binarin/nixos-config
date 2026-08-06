//! Proxmox interaction over plain SSH (no API client library).
//!
//! Mirrors ncf's approach: every `qm` command runs via
//! `ssh root@<host> qm ...` with shell-quoted args. Snippet uploads stream
//! over `ssh ... cat > <path>` (stdin pipe). Disk images go via `rsync -avP`.
//!
//! All methods are async (tokio) and surface stderr on failure.

use std::path::Path;

use anyhow::{Context, Result, bail};
use tokio::process::Command;

/// A Proxmox host reachable over SSH as `root@<host>`.
pub struct Proxmox {
    host: String,
}

impl Proxmox {
    pub fn new(host: impl Into<String>) -> Self {
        Self { host: host.into() }
    }

    /// Build an `ssh root@<host> <remote_cmd...>` argv (already shell-quoted
    /// via `shell_words::join` so values with spaces/parens survive the remote
    /// shell — ncf learned this the hard way with "xray-exit (residential...)").
    fn ssh_argv(&self, remote_cmd: &[String]) -> Vec<String> {
        let joined = shell_words::join(remote_cmd.iter().map(String::as_str));
        vec!["ssh".into(), format!("root@{}", self.host), joined]
    }

    /// Run `qm <args>` on the host. Returns combined stdout (trimmed).
    /// Errors include stderr for diagnostics.
    pub async fn qm(&self, args: &[&str]) -> Result<String> {
        let mut full: Vec<String> = vec!["qm".into()];
        full.extend(args.iter().map(|s| (*s).to_string()));
        self.run_remote(&full).await
    }

    /// Run an arbitrary remote command (argv already as Strings).
    pub async fn run_remote(&self, remote_cmd: &[String]) -> Result<String> {
        let argv = self.ssh_argv(remote_cmd);
        let output = Command::new(&argv[0])
            .args(&argv[1..])
            .output()
            .await
            .with_context(|| format!("spawning {:?}", argv))?;
        if !output.status.success() {
            bail!(
                "remote command failed on {}: {}\nstdout: {}\nstderr: {}",
                self.host,
                output.status,
                String::from_utf8_lossy(&output.stdout).trim(),
                String::from_utf8_lossy(&output.stderr).trim(),
            );
        }
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_owned())
    }

    /// Look up a VM by hostname. Returns its VMID if found.
    ///
    /// `qm list` columns: `VMID NAME STATUS MEM(MB) BOOTDISK(GB) PID`.
    pub async fn vmid_for_name(&self, name: &str) -> Result<Option<u64>> {
        let out = self.qm(&["list"]).await?;
        for line in out.lines().skip(1) {
            // Split on whitespace; first field is VMID, second is NAME.
            let mut it = line.split_whitespace();
            let vmid = it.next();
            let nm = it.next();
            if let (Some(vmid), Some(nm)) = (vmid, nm)
                && nm == name
                && let Ok(n) = vmid.parse::<u64>()
            {
                return Ok(Some(n));
            }
        }
        Ok(None)
    }

    /// Get the next free VMID from the cluster.
    pub async fn next_vmid(&self) -> Result<u64> {
        let out = self.qm(&["cluster", "nextid"]).await?;
        out.parse::<u64>()
            .with_context(|| format!("parsing nextid `{out}`"))
    }

    /// Check whether a file exists on the host (`test -e`).
    pub async fn file_exists(&self, remote_path: &str) -> Result<bool> {
        // `test -e` exits 0 if the file exists; we suppress the remote
        // shell's non-zero exit (which our `run_remote` would turn into an
        // error) by explicitly echoing the result.
        let out = self
            .run_remote(&["sh".into(), "-c".into(), format!("test -e {remote_path} && echo yes || echo no")])
            .await?;
        Ok(out.trim() == "yes")
    }

    /// Resolve the on-host directory for a snippet storage via `pvesm path`.
    /// E.g. `local:snippets/foo` -> `/var/lib/vz/snippets/foo`.
    pub async fn snippet_path(&self, storage: &str, filename: &str) -> Result<String> {
        let volid = format!("{storage}:snippets/{filename}");
        let dir = self
            .run_remote(&[
                "pvesm".into(),
                "path".into(),
                volid.clone(),
            ])
            .await
            .with_context(|| format!("resolving snippet volid `{volid}` (is `{storage}` a dir-type storage with snippets enabled?)"))?;
        Ok(dir)
    }

    /// Write `content` to `<path>` on the host via `ssh cat > <path>` (stdin).
    pub async fn write_file(&self, remote_path: &str, content: &str) -> Result<()> {
        // Use a quoted heredoc-free stream: `cat > <path>` with stdin.
        let mut cmd = Command::new("ssh");
        cmd.arg(format!("root@{}", self.host))
            .arg(format!("cat > {remote_path}"))
            .stdin(std::process::Stdio::piped());
        let mut child = cmd.spawn().context("spawning ssh for write_file")?;
        if let Some(mut stdin) = child.stdin.take() {
            use tokio::io::AsyncWriteExt;
            stdin.write_all(content.as_bytes()).await?;
        }
        let status = child.wait().await?;
        if !status.success() {
            bail!("ssh write_file to {remote_path} failed: {status}");
        }
        Ok(())
    }

    /// Copy a local file to the host via `rsync -avP`.
    pub async fn rsync_to(&self, local: &Path, remote_path: &str) -> Result<()> {
        let local_s = local
            .to_str()
            .context("local path is not valid UTF-8")?;
        let dest = format!("root@{}:{remote_path}", self.host);
        let status = Command::new("rsync")
            .args(["-avP", local_s, &dest])
            .status()
            .await
            .context("spawning rsync")?;
        if !status.success() {
            bail!("rsync {local_s} -> {dest} failed: {status}");
        }
        Ok(())
    }

    pub fn host(&self) -> &str {
        &self.host
    }
}
