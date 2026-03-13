"""Print Garage S3 credentials as AWS environment variables."""

import socket
from pathlib import Path
from typing import Optional

import typer
import yaml
from ..config import find_repo_root, get_machine_secrets_dir, user_binarin_yaml_path
from ..external import sops_decrypt_to_stdout, ExternalToolError
from ..output import console


def run(
    sops_file: Optional[Path] = None,
) -> None:
    """Print Garage S3 credentials as AWS environment variable exports.

    Args:
        sops_file: Path to sops-encrypted YAML file. If not provided,
                   uses secrets/<hostname>/user-binarin.yaml
    """
    if sops_file is None:
        hostname = socket.gethostname()
        repo_root = find_repo_root()
        machine_dir = get_machine_secrets_dir(hostname, repo_root)
        sops_file = user_binarin_yaml_path(machine_dir)

    if not sops_file.exists():
        raise ExternalToolError("sops", f"File does not exist: {sops_file}")

    # Decrypt and parse YAML
    decrypted = sops_decrypt_to_stdout(sops_file)
    data = yaml.safe_load(decrypted)

    # Extract garage credentials
    garage = data.get("garage")
    if not garage:
        raise ExternalToolError("sops", f"No 'garage' section found in {sops_file}")

    key_id = garage.get("key-id")
    secret_key = garage.get("secret-key")

    if not key_id:
        raise ExternalToolError("sops", f"No 'garage/key-id' found in {sops_file}")
    if not secret_key:
        raise ExternalToolError("sops", f"No 'garage/secret-key' found in {sops_file}")

    # Print export statements (to stdout for eval)
    print(f"export AWS_ACCESS_KEY_ID={key_id}")
    print(f"export AWS_SECRET_ACCESS_KEY={secret_key}")
    print("export AWS_REQUEST_CHECKSUM_CALCULATION=WHEN_REQUIRED")
    print("export AWS_RESPONSE_CHECKSUM_VALIDATION=WHEN_REQUIRED")
