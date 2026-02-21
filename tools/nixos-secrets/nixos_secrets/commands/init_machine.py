"""Initialize secrets for a new machine."""

import tempfile
from pathlib import Path

import git
from rich.console import Console
from rich.panel import Panel

from .. import config
from .. import external
from ..sops_yaml import SopsYaml

console = Console()


def run(name: str, no_user_key: bool = False, dry_run: bool = False) -> None:
    """Initialize secrets for a new machine.

    Steps:
    1. Create secrets/<machine>/ directory
    2. Check for existing secrets - handle migration gracefully
    3. Generate SSH host keys (if missing)
    4. Encrypt SSH private keys with sops
    5. Generate user age keypair (if missing and not --no-user-key)
    6. Encrypt user age key
    7. Derive server age key from SSH public key
    8. Update .sops.yaml with keys and creation rules
    9. Create empty YAML files (if missing)
    10. Stage all new files in git
    """
    console.print(Panel(f"Initializing secrets for machine: [bold]{name}[/bold]"))

    # Check external tools
    try:
        external.ensure_tools_available()
    except external.ExternalToolError as e:
        console.print(f"[red]{e}[/red]")
        return

    repo_root = config.find_repo_root()
    machine_dir = config.get_machine_secrets_dir(name, repo_root)

    if dry_run:
        console.print("[yellow]DRY RUN - no changes will be made[/yellow]\n")

    # Step 1: Create directory
    console.print(
        f"\n[bold]Step 1:[/bold] Creating directory {machine_dir.relative_to(repo_root)}"
    )
    if machine_dir.exists():
        console.print("  [dim]Directory already exists[/dim]")
    elif dry_run:
        console.print("  [yellow]Would create directory[/yellow]")
    else:
        machine_dir.mkdir(parents=True)
        console.print("  [green]Created directory[/green]")

    # Step 2: Check existing secrets
    console.print("\n[bold]Step 2:[/bold] Checking existing secrets")
    existing_ssh_keys = []
    existing_user_key = False

    for key_type in config.SSH_KEY_TYPES:
        key_path = config.ssh_host_key_path(machine_dir, key_type)
        pub_path = config.ssh_host_key_path(machine_dir, key_type, public=True)
        if key_path.exists() or pub_path.exists():
            existing_ssh_keys.append(key_type)
            console.print(f"  [dim]Found existing SSH {key_type} key[/dim]")

    age_key_path = config.user_age_key_path(machine_dir)
    if age_key_path.exists():
        existing_user_key = True
        console.print("  [dim]Found existing user age key[/dim]")

    if not existing_ssh_keys and not existing_user_key:
        console.print("  [dim]No existing secrets found[/dim]")

    # Step 3: Generate SSH host keys
    console.print("\n[bold]Step 3:[/bold] Generating SSH host keys")
    need_ssh_keys = not existing_ssh_keys or len(existing_ssh_keys) < len(
        config.SSH_KEY_TYPES
    )

    if not need_ssh_keys:
        console.print("  [dim]All SSH keys already exist, skipping generation[/dim]")
    elif dry_run:
        console.print("  [yellow]Would generate SSH host keys[/yellow]")
    else:
        external.ssh_keygen_generate_all(machine_dir)
        console.print("  [green]Generated SSH host keys[/green]")

    # Step 4: Encrypt SSH private keys
    console.print("\n[bold]Step 4:[/bold] Encrypting SSH private keys")
    for key_type in config.SSH_KEY_TYPES:
        key_path = config.ssh_host_key_path(machine_dir, key_type)
        if not key_path.exists():
            continue
        if external.is_sops_encrypted(key_path):
            console.print(f"  [dim]{key_path.name} already encrypted[/dim]")
        elif dry_run:
            console.print(f"  [yellow]Would encrypt {key_path.name}[/yellow]")
        else:
            external.sops_encrypt_inplace(key_path)
            console.print(f"  [green]Encrypted {key_path.name}[/green]")

    # Step 5 & 6: Generate and encrypt user age key
    user_age_pub = None
    if no_user_key:
        console.print(
            "\n[bold]Step 5-6:[/bold] [dim]Skipping user age key (--no-user-key)[/dim]"
        )
    else:
        console.print("\n[bold]Step 5:[/bold] Generating user age keypair")
        age_key_path = config.user_age_key_path(machine_dir)
        age_pub_path = config.user_age_key_path(machine_dir, public=True)

        if existing_user_key:
            console.print("  [dim]User age key already exists[/dim]")
            # Extract public key for .sops.yaml update
            if age_pub_path.exists():
                user_age_pub = age_pub_path.read_text().strip()
            elif external.is_sops_encrypted(age_key_path):
                # Need to decrypt to get public key
                try:
                    decrypted = external.sops_decrypt_to_stdout(age_key_path)
                    with tempfile.NamedTemporaryFile(
                        mode="w", suffix=".key", delete=False
                    ) as f:
                        f.write(decrypted)
                        temp_path = Path(f.name)
                    try:
                        user_age_pub = external.age_keygen_extract_public(temp_path)
                    finally:
                        temp_path.unlink()
                except Exception as e:
                    console.print(
                        f"  [yellow]Warning: Could not extract public key: {e}[/yellow]"
                    )
            else:
                user_age_pub = external.age_keygen_extract_public(age_key_path)
        elif dry_run:
            console.print("  [yellow]Would generate user age keypair[/yellow]")
        else:
            user_age_pub = external.age_keygen_generate(age_key_path)
            # Save public key to separate file
            age_pub_path.write_text(user_age_pub + "\n")
            console.print(f"  [green]Generated user age keypair[/green]")
            console.print(f"  [dim]Public key: {user_age_pub}[/dim]")

        console.print("\n[bold]Step 6:[/bold] Encrypting user age key")
        if not age_key_path.exists():
            console.print("  [dim]No key to encrypt[/dim]")
        elif external.is_sops_encrypted(age_key_path):
            console.print("  [dim]User age key already encrypted[/dim]")
        elif dry_run:
            console.print("  [yellow]Would encrypt user age key[/yellow]")
        else:
            external.sops_encrypt_inplace(age_key_path)
            console.print("  [green]Encrypted user age key[/green]")

    # Step 7: Derive server age key from SSH public key
    console.print("\n[bold]Step 7:[/bold] Deriving server age key from SSH ed25519 key")
    ed25519_pub = config.ssh_host_key_path(machine_dir, "ed25519", public=True)
    server_age_key = None

    if ed25519_pub.exists():
        if dry_run:
            console.print("  [yellow]Would derive server age key[/yellow]")
        else:
            server_age_key = external.ssh_to_age(ed25519_pub)
            console.print(f"  [green]Derived server age key[/green]")
            console.print(f"  [dim]{server_age_key}[/dim]")
    else:
        console.print("  [red]No SSH ed25519 public key found![/red]")

    # Step 8: Update .sops.yaml
    console.print("\n[bold]Step 8:[/bold] Updating .sops.yaml")
    if dry_run:
        console.print("  [yellow]Would update .sops.yaml with:[/yellow]")
        console.print(f"    - Key anchor: server_{name}")
        if user_age_pub:
            console.print(f"    - Key anchor: user_{name}_binarin")
        console.print(f"    - Creation rule for secrets/{name}/secrets.yaml")
        console.print(f"    - Creation rule for secrets/{name}/user-binarin.yaml")
    else:
        if server_age_key:
            sops = SopsYaml(config.get_sops_yaml_path(repo_root))
            sops.load()
            keys_added, rules_added = sops.update_machine_keys(
                name,
                server_age_key,
                user_age_pub,
            )
            sops.save()
            if keys_added:
                console.print("  [green]Added key anchors[/green]")
            else:
                console.print("  [dim]Key anchors already exist[/dim]")
            if rules_added:
                console.print("  [green]Added creation rules[/green]")
            else:
                console.print("  [dim]Creation rules already exist[/dim]")
        else:
            console.print("  [red]Skipping - no server age key[/red]")

    # Step 9: Create empty YAML files
    console.print("\n[bold]Step 9:[/bold] Creating empty secrets files")
    secrets_yaml = config.secrets_yaml_path(machine_dir)
    user_yaml = config.user_binarin_yaml_path(machine_dir)

    for yaml_path in [secrets_yaml, user_yaml]:
        if yaml_path.exists():
            console.print(f"  [dim]{yaml_path.name} already exists[/dim]")
        elif dry_run:
            console.print(f"  [yellow]Would create {yaml_path.name}[/yellow]")
        else:
            yaml_path.write_text("{}\n")
            console.print(f"  [green]Created {yaml_path.name}[/green]")

    # Step 10: Stage files in git
    console.print("\n[bold]Step 10:[/bold] Staging files in git")
    if dry_run:
        console.print("  [yellow]Would stage all new files[/yellow]")
    else:
        try:
            repo = git.Repo(repo_root)
            # Add all files in the machine's secrets directory
            repo.index.add([str(machine_dir.relative_to(repo_root))])
            # Also add .sops.yaml if modified
            sops_path = config.get_sops_yaml_path(repo_root)
            if repo.is_dirty(path=str(sops_path.relative_to(repo_root))):
                repo.index.add([str(sops_path.relative_to(repo_root))])
            console.print("  [green]Staged files in git[/green]")
        except Exception as e:
            console.print(f"  [yellow]Warning: Could not stage files: {e}[/yellow]")

    console.print("\n[bold green]Done![/bold green]")
    if not dry_run:
        console.print(f"\nNext steps:")
        console.print(f"  1. Review staged changes with: git status")
        console.print(f"  2. Edit secrets: sops secrets/{name}/secrets.yaml")
        console.print(f"  3. Commit when ready: git commit")
