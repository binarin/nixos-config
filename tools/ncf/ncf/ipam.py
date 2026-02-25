"""IPAM (IP Address Management) utilities for network allocation files."""

import ipaddress
from pathlib import Path
from typing import Any

import tomlkit
from tomlkit import TOMLDocument


def load_network_file(path: Path) -> TOMLDocument:
    """Load a network TOML file preserving formatting."""
    return tomlkit.parse(path.read_text())


def save_network_file(path: Path, doc: TOMLDocument) -> None:
    """Save a network TOML file."""
    path.write_text(tomlkit.dumps(doc))


def parse_ip(ip_str: str) -> ipaddress.IPv4Address:
    """Parse an IP address string."""
    return ipaddress.IPv4Address(ip_str)


def get_network_info(doc: TOMLDocument) -> dict[str, Any]:
    """Extract network info from document."""
    return dict(doc.get("info", {}))


def is_slash24_network(info: dict[str, Any]) -> bool:
    """Check if this is a /24 network."""
    return info.get("prefix") == 24


def get_all_ips_in_range(network_str: str) -> list[str]:
    """Get all IPs in a /24 network (1-254)."""
    base = ".".join(network_str.split(".")[:-1])
    return [f"{base}.{i}" for i in range(1, 255)]


def normalize_allocation(value: Any) -> dict[str, Any]:
    """Convert allocation to new hash format.

    Input formats:
    - "hostname" -> { hostname: "hostname", tags: ["primary"] }
    - ["hostname", "tag1", "tag2"] -> { hostname: "hostname", tags: ["tag1", "tag2"] }
    - { hostname: "...", tags: [...] } -> unchanged

    Output: always { hostname: "...", tags: [...] }
    """
    if isinstance(value, str):
        return {"hostname": value, "tags": ["primary"]}
    elif isinstance(value, list):
        return {"hostname": value[0], "tags": list(value[1:])}
    elif isinstance(value, dict):
        return dict(value)
    else:
        raise ValueError(f"Unknown allocation format: {value}")


def is_simple_allocation(alloc: dict[str, Any]) -> bool:
    """Check if allocation can use simple string format (hostname only with primary tag)."""
    return alloc["tags"] == ["primary"]


def format_ipam_section(
    ipam: dict[str, Any], network_info: dict[str, Any]
) -> tomlkit.items.Table:
    """Format IPAM section with sorted IPs and unallocated comments.

    Args:
        ipam: The current IPAM data (IP -> allocation mapping)
        network_info: Network info containing 'network' and 'prefix'

    Returns:
        A new tomlkit Table with formatted IPAM data
    """
    new_ipam = tomlkit.table()

    # Normalize all allocations to new format
    allocations: dict[str, dict[str, Any]] = {}
    for ip, value in ipam.items():
        allocations[ip] = normalize_allocation(value)

    # Get sorted list of all allocated IPs
    allocated_ips = sorted(allocations.keys(), key=lambda x: parse_ip(x))

    # Check if we should add unallocated comments
    add_unallocated = is_slash24_network(network_info)

    if add_unallocated:
        network_str = network_info["network"]
        all_ips = get_all_ips_in_range(network_str)
        all_ips_set = set(all_ips)
        allocated_set = set(allocated_ips)

        # Process all IPs in order
        for ip in all_ips:
            if ip in allocated_set:
                alloc = allocations[ip]
                if is_simple_allocation(alloc):
                    # Simple format: just hostname
                    new_ipam.add(ip, alloc["hostname"])
                else:
                    # Complex format: hash with hostname and tags
                    inline = tomlkit.inline_table()
                    inline["hostname"] = alloc["hostname"]
                    inline["tags"] = alloc["tags"]
                    new_ipam.add(ip, inline)
            else:
                # Unallocated IP - add as comment
                new_ipam.add(tomlkit.comment(f'"{ip}" = # unallocated'))
    else:
        # Not a /24 network - just sort and format without unallocated comments
        for ip in allocated_ips:
            alloc = allocations[ip]
            if is_simple_allocation(alloc):
                new_ipam.add(ip, alloc["hostname"])
            else:
                inline = tomlkit.inline_table()
                inline["hostname"] = alloc["hostname"]
                inline["tags"] = alloc["tags"]
                new_ipam.add(ip, inline)

    return new_ipam


def format_network_file(doc: TOMLDocument) -> TOMLDocument:
    """Format an entire network file with sorted IPs and unallocated comments."""
    info = get_network_info(doc)
    ipam = dict(doc.get("ipam", {}))

    # Create new document preserving info section
    new_doc = tomlkit.document()
    new_doc.add("info", doc["info"])
    new_doc.add(tomlkit.nl())
    new_doc.add("ipam", format_ipam_section(ipam, info))

    return new_doc


def find_next_available_ip(ipam: dict[str, Any], network_prefix: str) -> str | None:
    """Find the first available IP in the network.

    Scans from .2 to .254 (skipping .1 which is typically gateway).
    """
    base = ".".join(network_prefix.split(".")[:-1])
    used_ips = set(ipam.keys())

    for i in range(2, 255):
        ip = f"{base}.{i}"
        if ip not in used_ips:
            return ip

    return None


def add_allocation(
    doc: TOMLDocument,
    ip: str,
    hostname: str,
    tags: list[str] | None = None,
) -> TOMLDocument:
    """Add an IP allocation and reformat the file.

    Args:
        doc: The network document
        ip: IP address to allocate
        hostname: Hostname to assign
        tags: Optional list of tags (defaults to ["primary"])

    Returns:
        Reformatted document with new allocation
    """
    if tags is None:
        tags = ["primary"]

    # Add allocation to ipam
    ipam = dict(doc.get("ipam", {}))
    if tags == ["primary"]:
        ipam[ip] = hostname
    else:
        ipam[ip] = {"hostname": hostname, "tags": tags}

    # Update document and reformat
    doc["ipam"] = ipam
    return format_network_file(doc)
