"""Tailscale operations for ncf.

This module provides commands for interacting with the Tailscale API,
particularly for creating auth keys programmatically without using the web UI.
"""

import base64
import json
import subprocess
import urllib.request
import urllib.error
from pathlib import Path
from typing import Optional

from ..external import (
    register_tool,
    sops_decrypt_to_stdout,
    sops_encrypt_inplace,
    ExternalToolError,
)
from ..output import console

# Register fzf as an external tool dependency
register_tool("fzf", "Interactive tag selection for auth key creation")

# Tailscale API endpoints
TAILSCALE_API_BASE = "https://api.tailscale.com/api/v2"
OAUTH_TOKEN_URL = f"{TAILSCALE_API_BASE}/oauth/token"
TAILNET_KEYS_URL = f"{TAILSCALE_API_BASE}/tailnet/-/keys"


class TailscaleAPIError(Exception):
    """Error from Tailscale API."""

    def __init__(self, message: str, status_code: int = 0):
        self.status_code = status_code
        super().__init__(message)


def get_oauth_token(client_id: str, client_secret: str) -> str:
    """Exchange OAuth credentials for an access token.

    Args:
        client_id: Tailscale OAuth client ID
        client_secret: Tailscale OAuth client secret

    Returns:
        The access token string

    Raises:
        TailscaleAPIError: If the API request fails
    """
    # Prepare Basic auth header
    credentials = f"{client_id}:{client_secret}"
    auth_header = base64.b64encode(credentials.encode()).decode()

    # Prepare request
    data = b"grant_type=client_credentials"
    headers = {
        "Authorization": f"Basic {auth_header}",
        "Content-Type": "application/x-www-form-urlencoded",
    }

    req = urllib.request.Request(OAUTH_TOKEN_URL, data=data, headers=headers)

    try:
        with urllib.request.urlopen(req) as response:
            result = json.loads(response.read().decode())
            return result["access_token"]
    except urllib.error.HTTPError as e:
        body = e.read().decode() if e.fp else ""
        raise TailscaleAPIError(f"Failed to get OAuth token: {body}", e.code)
    except urllib.error.URLError as e:
        raise TailscaleAPIError(f"Network error: {e.reason}")


def get_available_tags(token: str) -> list[str]:
    """Get available tags from the OAuth client's capabilities.

    The Tailscale API doesn't have a direct endpoint to list available tags.
    Tags are determined by what the OAuth client was configured with in the
    admin console. We attempt to get this info from the keys endpoint's
    error response when providing invalid tags, or from listing existing keys.

    For now, we'll try to fetch existing keys and extract tags from them,
    or return an empty list if no keys exist.

    Args:
        token: OAuth access token

    Returns:
        List of available tag strings (e.g., ["tag:server", "tag:ci"])
    """
    # List existing keys to see what tags are available
    headers = {
        "Authorization": f"Bearer {token}",
    }

    req = urllib.request.Request(TAILNET_KEYS_URL, headers=headers, method="GET")

    try:
        with urllib.request.urlopen(req) as response:
            result = json.loads(response.read().decode())
            # Extract unique tags from existing keys
            tags = set()
            for key in result.get("keys", []):
                caps = key.get("capabilities", {})
                devices = caps.get("devices", {})
                create = devices.get("create", {})
                for tag in create.get("tags", []):
                    tags.add(tag)
            return sorted(tags)
    except urllib.error.HTTPError:
        # If we can't list keys, return empty list
        return []
    except urllib.error.URLError:
        return []


def create_auth_key(
    token: str,
    tags: list[str],
    reusable: bool = False,
    ephemeral: bool = True,
    preauthorized: bool = True,
    expiry_seconds: int = 3600,
    description: str = "ncf-generated",
) -> str:
    """Create an auth key via the Tailscale API.

    Args:
        token: OAuth access token
        tags: List of tags to apply (e.g., ["tag:server"])
        reusable: If True, the key can be used multiple times
        ephemeral: If True, devices using this key are ephemeral
        preauthorized: If True, devices are pre-authorized
        expiry_seconds: Key expiry time (0 for no expiry)
        description: Human-readable description

    Returns:
        The auth key string (tskey-auth-...)

    Raises:
        TailscaleAPIError: If the API request fails
    """
    payload = {
        "capabilities": {
            "devices": {
                "create": {
                    "reusable": reusable,
                    "ephemeral": ephemeral,
                    "preauthorized": preauthorized,
                    "tags": tags,
                }
            }
        },
        "expirySeconds": expiry_seconds,
        "description": description,
    }

    headers = {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }

    data = json.dumps(payload).encode()
    req = urllib.request.Request(TAILNET_KEYS_URL, data=data, headers=headers)

    try:
        with urllib.request.urlopen(req) as response:
            result = json.loads(response.read().decode())
            return result["key"]
    except urllib.error.HTTPError as e:
        body = e.read().decode() if e.fp else ""
        raise TailscaleAPIError(f"Failed to create auth key: {body}", e.code)
    except urllib.error.URLError as e:
        raise TailscaleAPIError(f"Network error: {e.reason}")


def select_tags_with_fzf(
    available_tags: list[str],
    preselect: Optional[list[str]] = None,
) -> list[str]:
    """Run fzf to interactively select tags.

    Args:
        available_tags: List of available tags to choose from
        preselect: Tags to pre-select (defaults to tag:server if available)

    Returns:
        List of selected tags

    Raises:
        ExternalToolError: If fzf fails or is not available
    """
    if not available_tags:
        return []

    # Default preselection: tag:server if available
    if preselect is None:
        preselect = ["tag:server"] if "tag:server" in available_tags else []

    # Build fzf input: mark preselected items
    # fzf --multi with +s disables sorting, --bind handles preselection
    input_text = "\n".join(available_tags)

    # Build fzf command with preselection
    # We use --bind to select items that match our preselect list
    fzf_cmd = [
        "fzf",
        "--multi",
        "--header=Select tags for auth key (TAB to select, ENTER to confirm)",
        "--prompt=Tags> ",
        "--height=~50%",
        "--border",
    ]

    # For preselection, we pipe the input with preselected items first
    # and use --bind 'start:select-all' only if we want all preselected
    # Actually, fzf doesn't have a clean way to preselect specific items
    # So we'll show the preselect info in the header instead
    if preselect:
        preselect_str = ", ".join(preselect)
        fzf_cmd[3] = f"--header=Select tags (suggested: {preselect_str})"

    try:
        result = subprocess.run(
            fzf_cmd,
            input=input_text,
            capture_output=True,
            text=True,
        )

        if result.returncode == 130:
            # User cancelled with Ctrl-C or Esc
            raise ExternalToolError("fzf", "Selection cancelled by user")
        elif result.returncode != 0:
            # Other error
            raise ExternalToolError(
                "fzf", f"fzf failed: {result.stderr}", result.returncode
            )

        # Parse selected tags (one per line)
        selected = [
            line.strip() for line in result.stdout.strip().split("\n") if line.strip()
        ]
        return selected

    except FileNotFoundError:
        raise ExternalToolError("fzf", "fzf not found in PATH")


def load_oauth_credentials(secrets_file: Optional[Path] = None) -> tuple[str, str]:
    """Load OAuth credentials from sops-encrypted secrets file.

    Args:
        secrets_file: Path to secrets file (default: secrets/tailscale/oauth.yaml)

    Returns:
        Tuple of (client_id, client_secret)

    Raises:
        ExternalToolError: If secrets file doesn't exist or credentials are missing
    """
    from .. import config  # Import here to avoid circular imports

    if secrets_file is None:
        secrets_file = config.get_secrets_dir() / "tailscale" / "oauth.yaml"

    if not secrets_file.exists():
        raise ExternalToolError(
            "sops",
            f"Secrets file not found: {secrets_file}\n"
            "Create it with:\n"
            f"  mkdir -p {secrets_file.parent}\n"
            f"  touch {secrets_file}\n"
            f"  ncf secrets set {secrets_file} tailscale/oauth_client_id\n"
            f"  ncf secrets set {secrets_file} tailscale/oauth_client_secret",
        )

    # Decrypt and parse YAML
    import yaml

    decrypted = sops_decrypt_to_stdout(secrets_file)
    data = yaml.safe_load(decrypted)

    tailscale_data = data.get("tailscale", {})
    client_id = tailscale_data.get("oauth_client_id")
    client_secret = tailscale_data.get("oauth_client_secret")

    if not client_id or not client_secret:
        raise ExternalToolError(
            "sops",
            f"Missing OAuth credentials in {secrets_file}\n"
            "Set them with:\n"
            f"  ncf secrets set {secrets_file} tailscale/oauth_client_id\n"
            f"  ncf secrets set {secrets_file} tailscale/oauth_client_secret",
        )

    return client_id, client_secret


def run_auth_key(
    reusable: bool = False,
    ephemeral: bool = True,
    preauthorized: bool = True,
    expiry_seconds: int = 3600,
    tags: Optional[list[str]] = None,
    secrets_file: Optional[Path] = None,
    description: str = "ncf-generated",
    machine: Optional[str] = None,
    dry_run: bool = False,
) -> None:
    """Create a Tailscale auth key.

    This command:
    1. Loads OAuth credentials from sops-encrypted secrets
    2. Gets an OAuth access token
    3. Either uses provided tags or shows fzf for interactive selection
    4. Creates the auth key and outputs it to stdout

    Args:
        reusable: If True, the key can be used multiple times
        ephemeral: If True, devices using this key are ephemeral
        preauthorized: If True, devices are pre-authorized
        expiry_seconds: Key expiry time in seconds (0 for no expiry)
        tags: Additional tags to apply (tag:server is always included)
        secrets_file: Path to OAuth secrets file
        description: Human-readable description for the key
        dry_run: Show what would be done without making changes
    """
    # Load credentials
    if dry_run:
        console.print(
            f"[yellow]Would load OAuth credentials from: {secrets_file or 'secrets/tailscale/oauth.yaml'}[/yellow]"
        )
        client_id = "dry-run-client-id"
        client_secret = "dry-run-secret"
    else:
        client_id, client_secret = load_oauth_credentials(secrets_file)

    # Get access token
    if dry_run:
        console.print(
            "[yellow]Would exchange OAuth credentials for access token[/yellow]"
        )
        token = "dry-run-token"
    else:
        console.print("[dim]Authenticating with Tailscale API...[/dim]")
        try:
            token = get_oauth_token(client_id, client_secret)
        except TailscaleAPIError as e:
            console.print(f"[red]Error: {e}[/red]")
            raise SystemExit(1)

    # Build tag list: start with provided tags, always include tag:server
    selected_tags = list(tags or [])
    if "tag:server" not in selected_tags:
        selected_tags.append("tag:server")

    # Create auth key
    if dry_run:
        console.print(f"[yellow]Would create auth key with:[/yellow]")
        console.print(f"[yellow]  - Tags: {selected_tags}[/yellow]")
        console.print(f"[yellow]  - Reusable: {reusable}[/yellow]")
        console.print(f"[yellow]  - Ephemeral: {ephemeral}[/yellow]")
        console.print(f"[yellow]  - Preauthorized: {preauthorized}[/yellow]")
        console.print(f"[yellow]  - Expiry: {expiry_seconds}s[/yellow]")
        console.print(f"[yellow]  - Description: {description}[/yellow]")
        if machine:
            console.print(
                f"[yellow]  - Target: secrets/{machine}/tailscale-auth[/yellow]"
            )
        else:
            console.print(
                "[yellow]tskey-auth-XXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXX[/yellow]"
            )
        return

    console.print(f"[dim]Creating auth key with tags: {selected_tags}[/dim]")
    try:
        key = create_auth_key(
            token=token,
            tags=selected_tags,
            reusable=reusable,
            ephemeral=ephemeral,
            preauthorized=preauthorized,
            expiry_seconds=expiry_seconds,
            description=description,
        )
    except TailscaleAPIError as e:
        console.print(f"[red]Error: {e}[/red]")
        raise SystemExit(1)

    if machine:
        # Write to separate sops-encrypted file
        machine_dir = Path(f"secrets/{machine}")
        if not machine_dir.exists():
            console.print(f"[red]Error: {machine_dir} does not exist[/red]")
            raise SystemExit(1)
        tailscale_auth_file = machine_dir / "tailscale-auth"
        tailscale_auth_file.write_text(key)
        sops_encrypt_inplace(tailscale_auth_file)
        console.print(f"[green]Auth key saved to {tailscale_auth_file}[/green]")
    else:
        # Output the key (to stdout, not stderr, so it can be captured)
        print(key)
