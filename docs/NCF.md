# NCF - NixOS Configuration Tool

`ncf` is a CLI tool for managing this NixOS configuration repository. It provides commands for building, evaluating, managing secrets, adding machines, and more.

## External Dependencies

The `ncf` tool relies on external command-line tools that must be available in PATH. This section explains how dependencies are managed and how to add new ones.

### Tool Registration System

External tool dependencies are declared using `register_tool()` in `tools/ncf/ncf/external.py`. This enables:

- Automatic verification that required tools are available
- Documentation of which modules use which tools and why
- Test suite validation of the development environment

Example registration:

```python
from ncf.external import register_tool

# In a command module (e.g., ncf/commands/my_command.py)
register_tool("mytool", "Description of what this tool is used for")

# For optional tools that enhance functionality but aren't required:
register_tool("mytool", "Nice-to-have feature", required=False)
```

### Viewing Current Dependencies

To see all registered external tool dependencies:

```bash
ncf ci external-deps
```

This displays a table showing:

- Tool name
- Availability status (available/missing)
- Whether it's required or optional
- Which module registered it
- What it's used for

### Runtime Dependencies in devshell.nix

External tools are provided via the Nix development shell. The `ncfRuntimeDeps` list in `modules/devshell.nix` defines tools that are:

1. Wrapped into the `ncf` executable (always available when ncf runs)
2. Also available in the development shell for direct use

Current runtime dependencies:

```nix
ncfRuntimeDeps = with pkgs; [
  nix              # Core nix commands
  nix-output-monitor # nom for nicer build output
  git              # Git operations
  git-crypt        # Encrypted file handling
  openssh          # ssh-keygen for host keys
  age              # age-keygen for user keys
  sops             # Secrets encryption/decryption
  ssh-to-age       # Convert SSH keys to age keys
  yamlfmt          # YAML file formatting
  apg              # Password generation
];
```

### Python Dependencies

Python library dependencies are specified in `modules/devshell.nix` in the `ncf` package definition:

```nix
dependencies = with pkgs.python3.pkgs; [
  pyyaml
  typer
  rich
  ruamel-yaml
  pydantic
  gitpython
  jinja2
  tomlkit
  proxmoxer
  paramiko
];
```

### Adding New External Dependencies

When adding a new external tool dependency:

1. **Register it in your module:**

   ```python
   from ncf.external import register_tool
   register_tool("newtool", "What this tool does in your module")
   ```

2. **Add to devshell.nix:**
   Add the package to `ncfRuntimeDeps`:

   ```nix
   ncfRuntimeDeps = with pkgs; [
     # ... existing tools ...
     newtool  # Add your new tool
   ];
   ```

3. **Verify availability:**

   ```bash
   ncf ci external-deps
   ```

4. **Run the test suite:**
   ```bash
   nix develop --ignore-env -c pytest tools/ncf/tests/test_external_deps.py
   ```

### Testing External Dependencies

The test suite in `tools/ncf/tests/test_external_deps.py` verifies:

- All required tools are available in PATH
- All optional tools are available (warns if missing)

Run tests with:

```bash
nix develop --ignore-env -c pytest tools/ncf/tests/test_external_deps.py
```

## Command Reference

### Build Commands

```bash
ncf build nixos <configuration>   # Build a NixOS configuration
ncf build home <host> <user>      # Build a home-manager configuration
ncf build lxc <target>            # Build an LXC tarball
ncf build iso                      # Build ISO image
ncf build all                      # Build all configurations in parallel
```

### Eval Commands

```bash
ncf eval nixos <configuration>    # Evaluate configuration (find errors)
ncf eval all                      # Evaluate all configurations
ncf eval query <config> <attr>    # Query specific attribute
```

### Secrets Commands

```bash
ncf secrets init-machine <name>   # Initialize secrets for new machine
ncf secrets list                  # List machines and secrets status
ncf secrets verify [machine]      # Verify secrets integrity
ncf secrets set <file> <path>     # Generate and set a secret
ncf secrets aws-env               # Print Garage S3 credentials
ncf secrets show-keys <machine>   # Show derived age keys
```

### Machine Commands

```bash
ncf machine add <name>            # Add new machine configuration
ncf machine provision <name>      # Provision Proxmox LXC container
```

### IP Address Management

```bash
ncf ipam format [network]         # Format network allocation files
```

### CI Commands

```bash
ncf ci matrix                     # Output CI build matrix JSON
ncf ci fake-unlock                # Replace encrypted files with placeholders
ncf ci build-path <name>          # Output nix build path for configuration
ncf ci external-deps              # Display external tool dependencies
```

### ISO Commands

```bash
ncf iso build-wifi                # Build ISO with WiFi credentials
```

## Common Workflows

### Adding a New Machine

```bash
# Add machine with IP allocation
ncf machine add myserver --network home

# Add LXC container
ncf machine add mylxc --type lxc --network home

# Skip IP allocation
ncf machine add myserver --no-network
```

### Building and Testing

```bash
# Quick validation
ncf eval nixos  # Current machine

# Full validation (takes time)
ncf eval all

# Build current machine
ncf build nixos $(hostname)

# Build all configurations
ncf build all
```

### Managing Secrets

```bash
# Initialize secrets for new machine
ncf secrets init-machine myserver

# Generate a new password and store it
ncf secrets set secrets/myserver/secrets.yaml myservice/password

# Verify all secrets are valid
ncf secrets verify --all
```
