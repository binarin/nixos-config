use anyhow::Result;
use clap::{Parser, Subcommand};

/// nixos-config-tool: CLI for NixOS configuration management.
#[derive(Debug, Parser)]
#[command(name = "nct", version, about, long_about = None)]
struct Cli {
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
        }
    }

    #[test]
    fn rejects_missing_subcommand() {
        assert!(Cli::try_parse_from(["nct"]).is_err());
    }
}
