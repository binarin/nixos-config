"""CI workflow generation for ncf."""

import base64
import io
import json
from pathlib import Path

from rich.console import Console
from ruamel.yaml import YAML

from .. import config
from ..external import run_command

console = Console()


def get_git_crypt_encrypted_files() -> list[Path]:
    """Get list of git-crypt encrypted files."""
    repo_root = config.find_repo_root()
    result = run_command(["git", "crypt", "status", "-e"], cwd=repo_root)
    files = []
    for line in result.stdout.strip().split("\n"):
        if line.strip():
            # Format is "    encrypted: path/to/file"
            parts = line.split(":", 1)
            if len(parts) == 2:
                filepath = parts[1].strip()
                files.append(repo_root / filepath)
    return files


def generate_fake_jwe() -> str:
    """Generate a valid-looking JWE compact serialization placeholder.

    JWE format: header.encrypted_key.iv.ciphertext.tag
    All parts are base64url encoded.
    """
    # Header: {"alg":"dir","enc":"A256GCM"} - direct encryption with AES-256-GCM
    header = (
        base64.urlsafe_b64encode(b'{"alg":"dir","enc":"A256GCM"}').decode().rstrip("=")
    )
    # For direct encryption, encrypted_key is empty
    encrypted_key = ""
    # IV: 12 bytes for AES-GCM, use zeros
    iv = base64.urlsafe_b64encode(b"\x00" * 12).decode().rstrip("=")
    # Ciphertext: some placeholder bytes
    ciphertext = (
        base64.urlsafe_b64encode(b"PLACEHOLDER_ENCRYPTED_DATA").decode().rstrip("=")
    )
    # Auth tag: 16 bytes for AES-GCM
    tag = base64.urlsafe_b64encode(b"\x00" * 16).decode().rstrip("=")

    return f"{header}.{encrypted_key}.{iv}.{ciphertext}.{tag}"


def generate_fake_content_for_file(filepath: Path) -> str:
    """Generate appropriate placeholder content based on file extension."""
    suffix = filepath.suffix.lower()
    name = filepath.name.lower()

    if suffix == ".jwe":
        return generate_fake_jwe()
    elif suffix == ".json":
        return "{}"
    elif ".git-crypt" in name or suffix == ".git-crypt":
        return "PLACEHOLDER_SECRET\n"
    else:
        return f"# Placeholder for {filepath.name}\n"


def run_fake_unlock() -> None:
    """Replace git-crypt encrypted files with placeholder content.

    This allows CI builds to proceed without real secrets.
    """
    encrypted_files = get_git_crypt_encrypted_files()

    if not encrypted_files:
        console.print("[yellow]No git-crypt encrypted files found[/yellow]")
        return

    console.print(
        f"[bold]Replacing {len(encrypted_files)} encrypted files with placeholders...[/bold]"
    )

    for filepath in encrypted_files:
        if not filepath.exists():
            console.print(f"[yellow]Skipping missing file: {filepath}[/yellow]")
            continue

        fake_content = generate_fake_content_for_file(filepath)
        filepath.write_text(fake_content)
        console.print(
            f"[green]✓[/green] {filepath.relative_to(config.find_repo_root())}"
        )

    console.print("\n[bold green]Fake unlock complete![/bold green]")


def get_nixos_configurations() -> list[str]:
    """Get list of all nixosConfigurations from the flake."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            ".#nixosConfigurations",
            "--apply",
            "builtins.attrNames",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_ci_config(configuration: str) -> dict:
    """Get CI configuration for a nixosConfiguration."""
    repo_root = config.find_repo_root()
    result = run_command(
        [
            "nix",
            "eval",
            "--json",
            f".#nixosConfigurations.{configuration}.config.ci",
        ],
        cwd=repo_root,
    )
    return json.loads(result.stdout)


def get_configurations_to_build() -> list[str]:
    """Get list of configurations that should be built by CI."""
    configs = get_nixos_configurations()
    result = []
    for cfg in configs:
        try:
            ci_config = get_ci_config(cfg)
            if ci_config.get("doBuild", True):
                result.append(cfg)
        except Exception as e:
            # If we can't get CI config, include it by default
            console.print(
                f"[yellow]Warning: Could not get CI config for {cfg}: {e}[/yellow]"
            )
            result.append(cfg)
    return result


def generate_checkout_and_unlock(ref: str | None = None) -> list[dict]:
    """Generate checkout and git-crypt unlock steps."""
    checkout_step = {"uses": "actions/checkout@v4"}
    if ref is not None:
        checkout_step["with"] = {"ref": ref}

    unlock_step = {
        "name": "unlock git-crypt",
        "env": {"GIT_CRYPT_KEY": "${{ secrets.GIT_CRYPT_KEY }}"},
        "run": 'git crypt unlock <(echo "$GIT_CRYPT_KEY"|base64 -d)',
    }

    return [checkout_step, unlock_step]


def generate_checkout_and_fake_unlock() -> list[dict]:
    """Generate checkout and fake unlock steps for fork PRs."""
    checkout_step = {
        "uses": "actions/checkout@v4",
        "with": {"ref": "${{ github.event.pull_request.head.sha }}"},
    }

    fake_unlock_step = {
        "name": "fake git-crypt unlock",
        "run": "nix run .#ncf -- ci fake-unlock",
    }

    return [checkout_step, fake_unlock_step]


def generate_master_yaml(configurations: list[str]) -> dict:
    """Generate the master workflow YAML data."""
    return {
        "on": {
            "push": {"branches": ["master"]},
        },
        "jobs": {
            "nixos-configuration": {
                "runs-on": "native",
                "strategy": {
                    "fail-fast": False,
                    "matrix": {"nixosConfiguration": configurations},
                },
                "steps": generate_checkout_and_unlock()
                + [
                    {
                        "run": "nix run .#ncf -- build nixos ${{ matrix.nixosConfiguration }} --no-nom"
                    }
                ],
            },
            "check": {
                "runs-on": "native",
                "needs": ["nixos-configuration"],
                "steps": generate_checkout_and_unlock() + [{"run": "nix flake check"}],
            },
        },
    }


def generate_fork_pr_yaml(configurations: list[str]) -> dict:
    """Generate the fork PR workflow YAML data.

    Uses pull_request_target to run on PRs from forks, with fake-unlock
    instead of real git-crypt secrets.
    """
    return {
        "on": {
            "pull_request_target": {"types": ["opened", "synchronize", "reopened"]},
        },
        "jobs": {
            "nixos-configuration": {
                "runs-on": "native",
                "strategy": {
                    "fail-fast": False,
                    "matrix": {"nixosConfiguration": configurations},
                },
                "steps": generate_checkout_and_fake_unlock()
                + [
                    {
                        "run": "nix run .#ncf -- build nixos ${{ matrix.nixosConfiguration }} --no-nom"
                    }
                ],
            },
            "check": {
                "runs-on": "native",
                "needs": ["nixos-configuration"],
                "steps": generate_checkout_and_fake_unlock()
                + [{"run": "nix flake check"}],
            },
        },
    }


def generate_docker_update_yaml() -> dict:
    """Generate the docker-update workflow YAML data."""
    return {
        "on": {
            "schedule": [{"cron": "42 11 * * *"}],
            "workflow_dispatch": {},
        },
        "jobs": {
            "propose-docker-updates": {
                "runs-on": "native",
                "steps": generate_checkout_and_unlock(ref="master")
                + [
                    {
                        "name": "Set git username for commits",
                        "run": 'git config user.name "Docker Image Updater" ',
                    },
                    {
                        "name": "Set git email for commits",
                        "run": 'git config user.email "docker-updater@binarin.info"',
                    },
                    {
                        "name": "API auth",
                        "run": 'set -x; fj -H forgejo.lynx-lizard.ts.net auth logout forgejo.lynx-lizard.ts.net || true; echo "${{ secrets.PR_TOKEN }}" | fj -H forgejo.lynx-lizard.ts.net auth add-key nixos-config-bumper',
                    },
                    {
                        "name": "Check for docker image updates and create PRs",
                        "run": """set -euo pipefail

# Run the check script in write mode
nix run .#check-arion-images -- --write || true

# Get list of changed JSON files
changed_files=$(git diff --name-only -- '*.json' || true)

if [[ -z "$changed_files" ]]; then
  echo "No docker image updates found"
  exit 0
fi

echo "Found updates in: $changed_files"

# Process each changed file
for json_file in $changed_files; do
  # Extract project name from filename (e.g., modules/machines/homebox.json -> homebox)
  project=$(basename "$json_file" .json)
  branch="docker-update-$project"

  echo "Processing $project..."

  # Stage just this file
  git add "$json_file"

  # Commit
  git commit -m "Update docker image versions for $project"

  # Push to branch (force to overwrite existing)
  git push --force origin "HEAD:$branch"

  # Create or update PR
  fj -H forgejo.lynx-lizard.ts.net pr create \\
    -r binarin/nixos-config \\
    --base master \\
    --head "$branch" \\
    --body "Automated docker image version bump for $project" \\
    "Update docker images: $project" || true

  # Reset back: undo the commit but keep changes staged
  git reset --soft HEAD~1
  # Unstage all changes
  git reset HEAD

  echo "Created/updated PR for $project"
done

echo "Done processing all updates"
""",
                    },
                ],
            }
        },
    }


def generate_iso_wifi_yaml() -> dict:
    """Generate the iso-wifi workflow YAML data."""
    return {
        "on": {"workflow_dispatch": {}},
        "jobs": {
            "build-iso-wifi": {
                "runs-on": "native",
                "if": "github.ref == 'refs/heads/master'",
                "steps": generate_checkout_and_unlock(ref="master")
                + [
                    {
                        "name": "Build WiFi-enabled ISO",
                        "run": "nix run .#ncf -- iso build-wifi --password-file files/agares-guest.git-crypt",
                    },
                ],
            }
        },
    }


def generate_flake_update_yaml(configurations: list[str]) -> dict:
    """Generate the flake-update workflow YAML data."""
    build_steps = [
        {
            "name": f"Build nixosConfiguration.{cfg}",
            "run": f"nix run .#ncf -- build nixos {cfg} --no-nom -o temp-result/{cfg}",
        }
        for cfg in configurations
    ]

    gc_root_steps = [
        {
            "name": f"Add nix-store GC root for nixosConfiguraion.{cfg}",
            "run": f'nix-store --add-root "$HOME/.cache/nixos-config/proposed-update/nixos-configuration/{cfg}" \\\n  -r "$(readlink -f "temp-result/{cfg}")"',
        }
        for cfg in configurations
    ]

    return {
        "on": {
            "schedule": [{"cron": "13 10 * * Mon"}],
            "workflow_dispatch": {},
        },
        "jobs": {
            "propose-inputs-update": {
                "runs-on": "native",
                "steps": generate_checkout_and_unlock(ref="master")
                + [
                    {"name": "Update flake inputs", "run": "nix flake update"},
                    {
                        "name": "give autofollow a chance to mess with flake.lock",
                        "run": "nix run .#write-flake",
                    },
                    {
                        "name": "Set git username for commits",
                        "run": 'git config user.name "Automatic Flake Updater" ',
                    },
                    {
                        "name": "Set git email for commits",
                        "run": 'git config user.email "flake-updater@binarin.info"',
                    },
                    {
                        "name": "Commit updates",
                        "run": 'git commit --allow-empty -am "Bump inputs"',
                    },
                ]
                + build_steps
                + [{"name": "Run flake check", "run": "nix flake check"}]
                + [
                    {
                        "name": "Clean-up old GC roots",
                        "run": 'rm -rf "$HOME/.cache/nixos-config/proposed-update/nixos-configuration"',
                    }
                ]
                + gc_root_steps
                + [
                    {
                        "name": "Push to flake-bump branch",
                        "run": "git push --force origin master:flake-bump",
                    },
                    {
                        "name": "API auth",
                        "run": 'set -x; fj -H forgejo.lynx-lizard.ts.net auth logout forgejo.lynx-lizard.ts.net || true; echo "${{ secrets.PR_TOKEN }}" | fj -H forgejo.lynx-lizard.ts.net auth add-key nixos-config-bumper',
                    },
                    {
                        "name": "Maybe create PR",
                        "run": 'fj -H forgejo.lynx-lizard.ts.net pr create -r binarin/nixos-config --base master --head flake-bump --body "Bump flake inputs" "Bump everything" || true',
                    },
                ],
            }
        },
    }


def _convert_multiline_strings(obj):
    """Recursively convert multiline strings to LiteralScalarString."""
    from ruamel.yaml.scalarstring import LiteralScalarString

    if isinstance(obj, dict):
        return {k: _convert_multiline_strings(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [_convert_multiline_strings(v) for v in obj]
    elif isinstance(obj, str) and "\n" in obj:
        return LiteralScalarString(obj)
    return obj


def dict_to_yaml(data: dict) -> str:
    """Convert dict to YAML string using ruamel.yaml."""
    yaml = YAML()
    yaml.default_flow_style = False
    yaml.width = 4096  # Prevent line wrapping
    # Convert multiline strings to literal block style
    data = _convert_multiline_strings(data)
    stream = io.StringIO()
    yaml.dump(data, stream)
    return stream.getvalue()


def run_generate(dry_run: bool = False) -> None:
    """Generate CI workflow YAML files."""
    repo_root = config.find_repo_root()
    workflows_dir = repo_root / ".forgejo" / "workflows"

    console.print("[bold]Fetching configurations to build...[/bold]")
    configurations = get_configurations_to_build()
    console.print(
        f"Found {len(configurations)} configurations to build: {', '.join(configurations)}"
    )

    # Check for excluded configurations
    all_configs = get_nixos_configurations()
    excluded = [c for c in all_configs if c not in configurations]
    if excluded:
        console.print(f"[yellow]Excluded from CI: {', '.join(excluded)}[/yellow]")

    workflows = {
        "master.yaml": generate_master_yaml(configurations),
        "fork-pr.yaml": generate_fork_pr_yaml(configurations),
        "docker-update.yaml": generate_docker_update_yaml(),
        "iso-wifi.yaml": generate_iso_wifi_yaml(),
        "flake-update.yaml": generate_flake_update_yaml(configurations),
    }

    header = "# auto-generated via: ncf ci generate\n"

    for filename, data in workflows.items():
        yaml_content = header + dict_to_yaml(data)
        filepath = workflows_dir / filename

        if dry_run:
            console.print(f"\n[bold]Would write to {filepath}:[/bold]")
            console.print(
                yaml_content[:500] + "..." if len(yaml_content) > 500 else yaml_content
            )
        else:
            filepath.write_text(yaml_content)
            console.print(f"[green]✓[/green] Generated {filepath}")

    if dry_run:
        console.print("\n[yellow]Dry run - no files written[/yellow]")
    else:
        console.print("\n[bold green]CI workflows generated successfully![/bold green]")
