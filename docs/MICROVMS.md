# MicroVM Setup for Coding Agents

This is an implementation of the microVM setup described in [Michael Stapelberg's blog post](https://michael.stapelberg.ch/posts/2026-02-01-coding-agent-microvm-nix/).

## Overview

This setup creates ephemeral virtual machines using `microvm.nix` to safely isolate coding agent execution. The VMs:

- Have no access to personal files on the host
- Are fully disposable and can be discarded if compromised
- Use cloud-hypervisor for lightweight virtualization
- Share the host's `/nix/store` via virtiofs for fast startup
- Have writable overlay for temporary modifications
- Run behind NAT on an isolated bridge network

## Architecture

### Network Configuration

- Bridge: `microbr` (192.168.83.1/24 subnet)
- NAT through host's WiFi interface
- Each VM gets a static IP (e.g., 192.168.83.2 for claudevm)
- DNS resolution via systemd-resolved

### VM Resources

- **vCPUs**: 8 cores per VM
- **RAM**: 4 GB per VM
- **Disk**: Ephemeral overlay (no persistent disk image)

### Shared Directories

Each VM has access to:

1. **Read-only Nix store**: `/nix/.ro-store` → host's `/nix/store`
2. **SSH keys**: `/etc/ssh/host-keys` → workspace's `ssh-host-keys/`
3. **Credentials**: `/home/binarin/claude-microvm` → shared credentials directory
4. **Workspace**: Project directory → mounted at same path in VM

## Creating a New VM

### Step 1: Create Workspace Directory

Run the helper script to set up a workspace for your VM:

```bash
sudo scripts/create-microvm-workspace.sh myvm
```

This creates:
- `/persist/home/binarin/workspaces/myvm/` - workspace directory
- `/persist/home/binarin/workspaces/myvm/ssh-host-keys/` - SSH keys directory
- SSH host keys for the VM
- `authorized_keys` file from your user's SSH keys

### Step 2: Add VM Configuration

Edit `modules/microvm-vms.nix` and add a new VM using the `mkMicroVM` helper:

```nix
# Example VM configuration
microvm.vms.myvm = mkMicroVM {
  name = "myvm";
  ip = "${cfg.subnet}.3";  # Use next available IP
  macAddress = "02:00:00:00:00:03";  # Use next available MAC
  workspace = "${cfg.workspaceBaseDir}/myvm";
  extraPackages = with pkgs; [
    nodejs
    python3
    go
    # Add any other packages your project needs
  ];
  # Optional: additional NixOS configuration
  extraConfig = {
    # environment.variables.MYVAR = "value";
  };
};
```

### Step 3: Rebuild System Configuration

```bash
# Build the configuration
sudo nixos-rebuild switch --flake .#furfur

# Or use your build script
just build
```

### Step 4: Start the VM

```bash
# Start the VM
sudo systemctl start microvm@myvm

# Check status
sudo systemctl status microvm@myvm

# View logs
sudo journalctl -u microvm@myvm -f
```

### Step 5: SSH to the VM

```bash
ssh 192.168.83.3  # Use the IP you assigned
```

## Running Claude Code in a VM

Once inside the VM:

```bash
# Navigate to your workspace
cd /persist/home/binarin/workspaces/myvm

# Clone your project (if not already there)
git clone <repo-url>

# Run Claude Code with dangerously-skip-permissions
# (safe because the VM has no access to sensitive data)
claude --dangerously-skip-permissions
```

## Configuration Files

- **modules/microvms.nix**: Base microVM infrastructure (bridge, NAT, common settings)
- **modules/microvm-vms.nix**: Individual VM definitions
- **modules/machines/furfur.nix**: Enables microVM support on furfur

## Customization

### Changing the Network Interface

If your WiFi interface is not `wlp0s20f3`, update it in `modules/machines/furfur.nix`:

```nix
nixos-config.microvms = {
  enable = true;
  hostInterface = "your-interface-name";  # Change this
};
```

Find your interface name with: `ip link show`

### Changing Default Paths

You can override the default paths in `modules/machines/furfur.nix`:

```nix
nixos-config.microvms = {
  enable = true;
  credentialsDir = "/path/to/credentials";
  workspaceBaseDir = "/path/to/workspaces";
};
```

## Managing VMs

```bash
# List all VMs
systemctl list-units 'microvm@*'

# Stop a VM
sudo systemctl stop microvm@myvm

# Restart a VM (fresh state)
sudo systemctl restart microvm@myvm

# Enable autostart on boot
sudo systemctl enable microvm@myvm
```

## Troubleshooting

### VM won't start

Check the journal for errors:
```bash
sudo journalctl -u microvm@myvm -n 100
```

### Can't SSH to VM

1. Verify the VM is running: `sudo systemctl status microvm@myvm`
2. Check if authorized_keys exists: `ls -la /persist/home/binarin/workspaces/myvm/ssh-host-keys/`
3. Verify network: `ping 192.168.83.3`

### VM has no internet

1. Check NAT is enabled: `sudo iptables -t nat -L`
2. Verify bridge: `ip addr show microbr`
3. Check host interface is correct in configuration

## Security Considerations

- VMs are ephemeral - no state persists across restarts except workspace directory
- VMs run behind NAT with no inbound connections
- Shared workspace is the only persistent storage
- Credentials directory keeps Claude config isolated from personal files
- Use `--dangerously-skip-permissions` safely since VMs have no access to sensitive data
