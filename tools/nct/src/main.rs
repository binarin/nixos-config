pub mod nix_eval;

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
}
