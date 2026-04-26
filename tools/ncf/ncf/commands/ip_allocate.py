"""Allocate an IP for a machine in a network."""

from pathlib import Path

from tomlkit import TOMLDocument

from .. import config as ncf_config
from .. import ipam as ipam_utils
from ..nix import NixRunner
from ..output import console


def generate_mac_from_host_id(host_id: str) -> str:
    """Generate a locally administered MAC address from host ID.

    Uses the host ID (4 bytes / 8 hex chars) as the last 4 octets of the MAC.
    The first byte is 02 (locally administered, unicast).
    Format: 02:00:XX:XX:XX:XX where XX:XX:XX:XX is the host ID.
    """
    octets = [host_id[i : i + 2] for i in range(0, 8, 2)]
    return f"02:00:{octets[0]}:{octets[1]}:{octets[2]}:{octets[3]}".upper()


def find_existing_allocation(
    ipam_data: dict, hostname: str, tags: list[str]
) -> str | None:
    """Find if hostname is already allocated with matching tags.

    Returns the IP if found, None otherwise.
    """
    for ip, value in ipam_data.items():
        normalized = ipam_utils.normalize_allocation(value)
        if normalized["hostname"] == hostname:
            existing_tags = normalized.get("tags", ["primary"])
            if sorted(existing_tags) == sorted(tags):
                return ip
    return None


def find_different_tags_allocation(
    ipam_data: dict, hostname: str, tags: list[str]
) -> tuple[str, list[str]] | None:
    """Find if hostname is allocated with different tags.

    Returns (ip, existing_tags) if found, None otherwise.
    """
    for ip, value in ipam_data.items():
        normalized = ipam_utils.normalize_allocation(value)
        if normalized["hostname"] == hostname:
            existing_tags = normalized.get("tags", ["primary"])
            if sorted(existing_tags) != sorted(tags):
                return (ip, existing_tags)
    return None


def _get_mac_from_host_id(repo_root: Path, hostname: str) -> str | None:
    """Try to derive a MAC address from the host's hostId.

    Evaluates `.#nixosConfigurations.<hostname>.config.networking.hostId`
    via nix eval. If the hostId is non-zero, returns the MAC in format
    02:00:XX:XX:XX:XX. If evaluation fails or the hostId is zero,
    returns None and prints a short warning.
    """
    try:
        runner = NixRunner(verbosity=0, repo_root=repo_root)
        result = runner.run_eval(
            f".#nixosConfigurations.{hostname}.config.networking.hostId",
            raw=True,
        )
    except Exception as e:
        console.print(
            f"  [yellow]Warning: Could not evaluate hostId for "
            f"'{hostname}': {e}, skipping MAC generation[/yellow]"
        )
        return None

    host_id = result.stdout.strip()

    if not host_id or host_id == "00000000":
        return None

    # Validate that it looks like an 8-char hex string
    if len(host_id) != 8 or not all(c in "0123456789abcdefABCDEF" for c in host_id):
        console.print(
            f"  [yellow]Warning: Unexpected hostId format '{host_id}' for "
            f"'{hostname}', skipping MAC generation[/yellow]"
        )
        return None

    return generate_mac_from_host_id(host_id)


def _update_mac_in_allocation(
    network_doc: TOMLDocument,
    network_path: Path,
    ip: str,
    hostname: str,
    tags: list[str],
    expected_mac: str,
    repo_root: Path,
    dry_run: bool,
) -> None:
    """Update the MAC address in an existing allocation if needed."""
    ipam_data = dict(network_doc.get("ipam", {}))
    value = ipam_data.get(ip)
    if value is None:
        return

    normalized = ipam_utils.normalize_allocation(value)
    existing_mac = normalized.get("mac")

    if existing_mac:
        if existing_mac.upper() == expected_mac.upper():
            console.print(f"  [green]MAC already correct: {existing_mac}[/green]")
        else:
            console.print(
                f"  [yellow]Warning: Existing MAC ({existing_mac}) doesn't match "
                f"expected MAC ({expected_mac}) for hostId, leaving untouched[/yellow]"
            )
        return

    # MAC not set — update the allocation with MAC
    console.print(f"  [green]Setting MAC to {expected_mac}[/green]")

    if dry_run:
        console.print("  [yellow]Would update MAC (dry run)[/yellow]")
        return

    formatted_doc = ipam_utils.add_allocation(
        network_doc, ip, hostname, tags=tags, mac=expected_mac
    )
    ipam_utils.save_network_file(network_path, formatted_doc)
    console.print(f"  [green]Updated {network_path.relative_to(repo_root)}[/green]")


def run(
    hostname: str,
    network: str = "home",
    tags: list[str] | None = None,
    dry_run: bool = False,
) -> None:
    """Allocate an IP for a machine in a network.

    Checks if the hostname is already allocated with the given tags.
    If so, reports the existing allocation and optionally updates the MAC.
    If not, finds the first available IP and allocates it.

    If the machine has a non-zero hostId (evaluated from
    `.#nixosConfigurations.<hostname>.config.networking.hostId`), the MAC
    address is automatically set to 02:00:<host-id-4-bytes> format. If the
    hostId cannot be evaluated a short warning is printed and MAC is left
    untouched.
    """
    if tags is None:
        tags = ["primary"]

    tag_info = f" (tags: {tags})" if tags != ["primary"] else ""
    console.print(
        f"[bold]Allocating IP for '{hostname}' in network '{network}'[/bold]{tag_info}"
    )

    repo_root = ncf_config.find_repo_root()

    # Load network file
    network_path = repo_root / "inventory" / "networks" / f"{network}.toml"
    if not network_path.exists():
        console.print(f"[red]Network file not found: {network_path}[/red]")
        raise SystemExit(1)

    network_doc = ipam_utils.load_network_file(network_path)
    network_info = ipam_utils.get_network_info(network_doc)
    network_prefix = network_info["network"]
    ipam_data = dict(network_doc.get("ipam", {}))

    # ── Check if already allocated with matching tags ──────────────────────────
    existing_ip = find_existing_allocation(ipam_data, hostname, tags)
    if existing_ip is not None:
        console.print(
            f"  [green]Already allocated: {existing_ip} -> {hostname}[/green]"
        )
        expected_mac = _get_mac_from_host_id(repo_root, hostname)
        if expected_mac is not None:
            _update_mac_in_allocation(
                network_doc=network_doc,
                network_path=network_path,
                ip=existing_ip,
                hostname=hostname,
                tags=tags,
                expected_mac=expected_mac,
                repo_root=repo_root,
                dry_run=dry_run,
            )
        return

    # ── Check if hostname is allocated with different tags ─────────────────────
    conflict = find_different_tags_allocation(ipam_data, hostname, tags)
    if conflict is not None:
        conflict_ip, existing_tags = conflict
        console.print(
            f"  [red]Hostname '{hostname}' is already allocated at {conflict_ip} "
            f"with different tags ({existing_tags} vs {tags})[/red]"
        )
        console.print("  Use matching tags or update the allocation manually.")
        raise SystemExit(1)

    # ── Find first available IP ────────────────────────────────────────────────
    allocated_ip = ipam_utils.find_next_available_ip(ipam_data, network_prefix)
    if not allocated_ip:
        console.print(f"  [red]No available IPs in network '{network}'[/red]")
        raise SystemExit(1)

    console.print(f"  [green]Found available IP: {allocated_ip}[/green]")

    # ── Derive MAC from hostId if available ────────────────────────────────────
    mac = _get_mac_from_host_id(repo_root, hostname)

    # ── Apply allocation ───────────────────────────────────────────────────────
    if dry_run:
        mac_info = f" (MAC: {mac})" if mac else ""
        console.print(
            f'  [yellow]Would allocate: {allocated_ip} = "{hostname}"{mac_info}[/yellow]'
        )
        console.print("\n[yellow]DRY RUN — no changes made[/yellow]")
        return

    formatted_doc = ipam_utils.add_allocation(
        network_doc, allocated_ip, hostname, tags=tags, mac=mac
    )
    ipam_utils.save_network_file(network_path, formatted_doc)

    mac_info = f" (MAC: {mac})" if mac else ""
    console.print(f'  [green]Allocated {allocated_ip} = "{hostname}"{mac_info}[/green]')
    console.print(f"  [green]Updated {network_path.relative_to(repo_root)}[/green]")

    console.print("\n[bold green]Done![/bold green]")
