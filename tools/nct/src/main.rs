pub mod nix_eval;
pub mod nix_flake;
pub mod proxmox;
pub mod config;
pub mod cloud_init;
pub mod provision_pve;

use anyhow::Result;
use clap::{Parser, Subcommand};

/// nixos-config-tool: CLI for NixOS configuration management.
#[derive(Debug, Parser)]
#[command(name = "nct", version, about, long_about = None)]
struct Cli {
    /// Flake root to evaluate (directory containing flake.nix).
    /// Defaults to the current working directory.
    #[arg(short, long, global = true, default_value = ".")]
    flake: String,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Machine management.
    Machine {
        #[command(subcommand)]
        command: MachineCommand,
    },
}

#[derive(Debug, Subcommand)]
enum MachineCommand {
    /// Provision a new machine.
    Provision {
        /// Name of the machine to provision.
        name: String,
    },

    /// Provision a cloud-image VM on Proxmox, with the clan age key injected
    /// via cloud-init (NoCloud seed) so `provision-clan-key` bootstraps
    /// clan/sops secrets on first boot.
    ///
    /// Builds `system.build.cloudImage` (qcow2), generates cloud-init snippets,
    /// creates the VM, imports the disk, and optionally starts it.
    ProvisionPve {
        /// Name of the machine (attr under nixosConfigurations).
        name: String,

        /// Proxmox host to provision on.
        #[arg(short, long)]
        proxmox_host: String,

        /// Inventory network to wire the VM into (e.g. `home`, `guest`).
        /// Determines IP, gateway, prefix, DNS, and VLAN tag.
        #[arg(long)]
        network: String,

        /// Physical bridge on the Proxmox side.
        #[arg(long, default_value = "vmbr0")]
        bridge: String,

        /// Dir-type storage for cloud-init snippets (resolved via `pvesm path`).
        #[arg(long, default_value = "local")]
        snippet_storage: String,

        /// Storage for the imported disk + cloud-init drive
        /// (defaults to the first disk's storage from config, else local-zfs).
        #[arg(long)]
        disk_storage: Option<String>,

        /// Start the VM after provisioning.
        #[arg(short, long)]
        start: bool,

        /// Show what would be done without executing.
        #[arg(long)]
        dry_run: bool,
    },

    /// Evaluate a Nix expression against a machine's full config.
    ///
    /// Equivalent to `nix eval .#nixosConfigurations.<name> --apply EXPR`,
    /// but performed in-process via the nix-bindings library.
    ///
    /// The lambda receives the whole `nixosConfigurations.<name>` value as
    /// its argument, so `m.config.X` is the access pattern.
    ///
    /// Example:
    ///   nct machine eval-expr llm-runner 'm: builtins.attrNames m.config.llama-models.configurations'
    EvalExpr {
        /// Name of the machine (attr under nixosConfigurations).
        name: String,
        /// Nix lambda applied to the machine value, e.g. `m: m.config.foo`.
        expr: String,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Machine { command } => match command {
            MachineCommand::Provision { name } => {
                println!("provisioning machine: {name}");
                // TODO: implement provisioning logic
            }
            MachineCommand::ProvisionPve {
                name,
                proxmox_host,
                network,
                bridge,
                snippet_storage,
                disk_storage,
                start,
                dry_run,
            } => {
                let flake = nix_flake::NixFlake::open(&cli.flake)?;
                provision_pve::run(
                    &flake,
                    provision_pve::ProvisionPveArgs {
                        machine: name,
                        proxmox_host,
                        network,
                        bridge,
                        snippet_storage,
                        disk_storage,
                        start,
                        dry_run,
                    },
                )
                .await?;
            }
            MachineCommand::EvalExpr { name, expr } => {
                let out = nix_eval::eval_machine_expr(&cli.flake, &name, &expr)?;
                print!("{out}");
            }
        },
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_machine_provision() {
        let cli = Cli::try_parse_from(["nct", "machine", "provision", "myhost"]).unwrap();
        match cli.command {
            Command::Machine {
                command: MachineCommand::Provision { name },
            } => assert_eq!(name, "myhost"),
            _ => panic!("expected Provision"),
        }
    }

    #[test]
    fn parse_machine_eval_expr() {
        let cli = Cli::try_parse_from([
            "nct",
            "machine",
            "eval-expr",
            "llm-runner",
            "m: builtins.attrNames m.config.x",
        ])
        .unwrap();
        match cli.command {
            Command::Machine {
                command: MachineCommand::EvalExpr { name, expr },
            } => {
                assert_eq!(name, "llm-runner");
                assert_eq!(expr, "m: builtins.attrNames m.config.x");
            }
            _ => panic!("expected EvalExpr"),
        }
    }

    #[test]
    fn rejects_missing_subcommand() {
        assert!(Cli::try_parse_from(["nct"]).is_err());
    }

    #[test]
    fn parse_machine_provision_pve() {
        let cli = Cli::try_parse_from([
            "nct",
            "machine",
            "provision-pve",
            "xray-exit",
            "-p",
            "valak",
            "--network",
            "guest",
        ])
        .unwrap();
        match cli.command {
            Command::Machine {
                command: MachineCommand::ProvisionPve {
                    name,
                    proxmox_host,
                    network,
                    bridge,
                    snippet_storage,
                    disk_storage,
                    start,
                    dry_run,
                },
            } => {
                assert_eq!(name, "xray-exit");
                assert_eq!(proxmox_host, "valak");
                assert_eq!(network, "guest");
                assert_eq!(bridge, "vmbr0");
                assert_eq!(snippet_storage, "local");
                assert_eq!(disk_storage, None);
                assert!(!start);
                assert!(!dry_run);
            }
            _ => panic!("expected ProvisionPve"),
        }
    }
}
