# Nix Flake Initialization - Technical Reference

## Flake-Parts Architecture

Flake-parts is a module system for Nix flakes that provides better organization and composability. Key concepts:

- **perSystem**: Defines per-system outputs (things that differ by architecture)
- **systems**: List of supported systems (e.g., `["x86_64-linux" "aarch64-darwin"]`)
- **mkFlake**: Main entry point that evaluates the module system

## DevShell Package Syntax

### Simple Packages
```nix
packages = with pkgs; [
  git
  curl
  jq
];
```

### Python with Packages
```nix
packages = with pkgs; [
  (python3.withPackages (ps: with ps; [
    requests
    flask
    pytest
  ]))
];
```

### Multiple Python Versions
```nix
packages = with pkgs; [
  python39
  (python311.withPackages (ps: with ps; [ django ]))
];
```

## Language Detection Patterns

### File Extension Mapping
| Language | Extensions | Manifest Files |
|----------|-----------|----------------|
| OpenSCAD | `.scad` | None |
| Python | `.py`, `.pyx` | `requirements.txt`, `pyproject.toml`, `setup.py`, `Pipfile` |
| Rust | `.rs` | `Cargo.toml`, `Cargo.lock` |
| JavaScript | `.js`, `.ts` | `package.json` |
| Go | `.go` | `go.mod` |

### Priority Rules
1. Always check for manifest files first (higher confidence)
2. Use file extensions as secondary confirmation
3. If both are present, language is definitely used
4. If only extensions found, check if >3 files (avoid false positives)

## Nixpkgs Python Package Mappings

### Common PyPI to Nixpkgs Mappings
| PyPI Name | Nixpkgs Name | Notes |
|-----------|--------------|-------|
| `qrcode` | `qrcode` | Direct match |
| `pillow` | `pillow` | Direct match |
| `beautifulsoup4` | `beautifulsoup4` | Direct match |
| `pyyaml` | `pyyaml` | Direct match |
| `scikit-learn` | `scikit-learn` | Keep hyphen |
| `opencv-python` | `opencv4` | Different name! |
| `python-dateutil` | `python-dateutil` | Direct match |

### Known Exceptions
- `opencv-python` → Use `opencv4` instead
- `PyQt5` → Use `pyqt5` (lowercase)
- `Pillow` → Use `pillow` (lowercase)

### Package Not Found Solutions
1. Search nixpkgs: `nix search nixpkgs python3Packages.<name>`
2. Check alternatives: `nix search nixpkgs <partial-name>`
3. Use pip in shell: Add `pip` to packages and install manually
4. Build from source: Use `buildPythonPackage` or `buildPythonApplication`

## Requirements.txt Parsing Rules

### Valid Line Formats
```txt
# Simple version pin
package-name==1.2.3

# Version ranges
package-name>=1.0.0
package-name<2.0.0
package-name>=1.0.0,<2.0.0

# Git URLs (skip these)
git+https://github.com/user/repo.git

# Local paths (skip these)
-e .
-e ./local-package

# Comments (skip)
# This is a comment

# Extras
package-name[extra1,extra2]==1.0.0
```

### Extraction Algorithm
```
for each line in requirements.txt:
    1. Strip whitespace
    2. Skip if empty or starts with '#'
    3. Skip if starts with '-e' or 'git+'
    4. Extract package name (everything before '==', '>=', '<', '[', etc.)
    5. Convert to lowercase
    6. Replace '-' with nothing (keep hyphens in nixpkgs names)
    7. Add to package list
```

## System Architecture Options

### Common Systems
```nix
systems = [
  "x86_64-linux"    # 64-bit Linux (Intel/AMD)
  "aarch64-linux"   # 64-bit Linux (ARM)
  "x86_64-darwin"   # macOS (Intel)
  "aarch64-darwin"  # macOS (Apple Silicon)
];
```

### When to Use Multiple Systems
- Open source projects with cross-platform contributors
- Projects deployed on different architectures
- CI/CD that runs on multiple platforms

### When to Use Single System
- Personal projects
- Organization with standardized hardware
- Performance-critical builds (faster evaluation)

## Nixpkgs Channel Selection

### Stable Channels (Recommended for Production)
- `nixos-25.11` - Current stable (as of 2025-12-04)
- `nixos-24.11` - Previous stable
- `nixos-24.05` - Older stable

### Rolling Channels
- `nixos-unstable` - Latest packages, bleeding edge
- `nixpkgs-unstable` - Similar but without NixOS tests

### Selection Criteria
| Use Case | Recommended Channel |
|----------|-------------------|
| Production/Stable Development | `nixos-25.11` |
| Need latest package versions | `nixos-unstable` |
| Contributing to nixpkgs | `nixos-unstable` |
| Maximum stability | Previous stable (e.g., `nixos-24.11`) |

## Direnv Integration

### .envrc Contents
```bash
# Basic usage
use flake

# With specific flake path
use flake .#default

# With logging
use flake --impure

# Load additional environment
use flake
export CUSTOM_VAR=value
```

### .gitignore Entries
```
# Direnv
.direnv/

# Nix
result
result-*

# Do NOT ignore
# flake.nix
# flake.lock
# .envrc
```

## Common Issues and Solutions

### Issue: "package X not found in python3Packages"
**Solution**:
1. Check exact package name: `nix search nixpkgs python311Packages.X`
2. Try hyphenated version: `python-X` instead of `X`
3. Add pip to packages and install manually in shell
4. Use override: `(python3.withPackages (ps: with ps; [ pip ])).override { inherit X; }`

### Issue: "system not supported"
**Solution**: Add more systems to the systems list or ask user which system they use

### Issue: "flake.lock conflicts"
**Solution**: Add to .gitattributes: `flake.lock merge=union`

### Issue: "direnv not activating"
**Solution**:
1. Check `direnv` is installed
2. Run `direnv allow`
3. Check `.envrc` is in project root
4. Ensure shell hook is configured: `eval "$(direnv hook bash)"`

## Testing the Generated Flake

**IMPORTANT**: Before testing, ensure flake.nix is tracked by Git:
```bash
# Stage the flake file first (required for Nix to see it)
git add flake.nix
```

After creating and staging flake.nix, verify with:

```bash
# Check flake syntax (requires flake.nix to be git-tracked)
nix flake check

# Show flake outputs
nix flake show

# Enter development shell
nix develop

# Build lock file
nix flake update

# Validate all dependencies resolve
nix flake metadata
```

**Note**: Nix flakes use the Git repository as the source of truth. Any files not tracked by Git (either staged or committed) will not be visible to Nix commands. This is why `git add flake.nix` must be run before `nix flake check` or other flake commands.
