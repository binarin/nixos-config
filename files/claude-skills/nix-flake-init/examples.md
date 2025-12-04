# Nix Flake Initialization - Usage Examples

## Example 1: OpenSCAD + Python Project (Current Project)

### Project Structure
```
qrcode_wifi_scad/
├── wifi-card.scad
├── qrcode-matrix.scad
├── generate.py
└── requirements.txt (contains: qrcode==7.4.2)
```

### Detection Process
1. Glob finds: `*.scad` files → OpenSCAD detected
2. Glob finds: `*.py` files → Python detected
3. Read `requirements.txt` → Extract: `qrcode`

### Generated flake.nix
```nix
{
  description = "Development environment for qrcode_wifi_scad";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          name = "default-dev-shell";
          meta.description = "Default development shell";
          packages = with pkgs; [
            openscad-unstable
            (python3.withPackages (ps: with ps; [ qrcode ]))
          ];
        };
      };
    };
}
```

### Created Files
- `flake.nix`
- `.envrc` (contains: `use flake`)

### User Instructions
```
Created flake.nix with OpenSCAD and Python support!

Next steps:
1. Run: nix flake update
2. Run: nix develop (or just cd into the directory if using direnv)
3. Run: direnv allow (to activate direnv)
4. Add to .gitignore: .direnv/
5. Commit: git add flake.nix flake.lock .envrc
```

---

## Example 2: Python Project with Multiple Dependencies

### Project Structure
```
data-analysis/
├── analyze.py
├── utils.py
├── tests/
│   └── test_analyze.py
└── requirements.txt
```

### requirements.txt Contents
```
pandas==2.1.0
numpy>=1.24.0
matplotlib==3.8.0
scikit-learn==1.3.0
pytest==7.4.2
```

### Detection Process
1. Glob finds: `*.py` files → Python detected
2. Read `requirements.txt` → Extract: pandas, numpy, matplotlib, scikit-learn, pytest

### Generated flake.nix
```nix
{
  description = "Development environment for data-analysis";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          name = "default-dev-shell";
          meta.description = "Default development shell";
          packages = with pkgs; [
            (python3.withPackages (ps: with ps; [
              pandas
              numpy
              matplotlib
              scikit-learn
              pytest
            ]))
          ];
        };
      };
    };
}
```

---

## Example 3: OpenSCAD-Only Project

### Project Structure
```
3d-models/
├── case.scad
├── lid.scad
└── parts/
    ├── hinge.scad
    └── clasp.scad
```

### Detection Process
1. Glob finds: `*.scad` files → OpenSCAD detected
2. No Python files or requirements.txt → Python not detected

### Generated flake.nix
```nix
{
  description = "Development environment for 3d-models";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          name = "default-dev-shell";
          meta.description = "Default development shell";
          packages = with pkgs; [
            openscad-unstable
          ];
        };
      };
    };
}
```

---

## Example 4: Python Project Without requirements.txt

### Project Structure
```
simple-script/
├── main.py
└── config.py
```

### Detection Process
1. Glob finds: `*.py` files → Python detected
2. No requirements.txt → Use base Python without extra packages

### Generated flake.nix
```nix
{
  description = "Development environment for simple-script";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          name = "default-dev-shell";
          meta.description = "Default development shell";
          packages = with pkgs; [
            python3
          ];
        };
      };
    };
}
```

---

## Example 5: Multi-Platform Project

### User Request
"Create a flake.nix for this project. I need it to work on Linux (x86_64) and macOS (Apple Silicon)."

### Generated flake.nix
```nix
{
  description = "Development environment for multi-platform-project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          name = "default-dev-shell";
          meta.description = "Default development shell";
          packages = with pkgs; [
            # detected packages here
          ];
        };
      };
    };
}
```

---

## Example 6: Complex requirements.txt Parsing

### requirements.txt Contents
```
# Core dependencies
requests==2.31.0
flask>=3.0.0,<4.0.0

# Development tools
pytest==7.4.2
black==23.9.1

# Optional dependencies
pillow[jpeg]==10.0.0

# Git dependencies (should be skipped)
git+https://github.com/user/custom-lib.git

# Local packages (should be skipped)
-e .
-e ./local-module

# Empty lines and comments


# More packages
pyyaml==6.0.1
```

### Extracted Packages
- requests
- flask
- pytest
- black
- pillow (extras ignored in basic parsing)
- pyyaml

### Skipped Entries
- Git URL (not in nixpkgs)
- Local packages (-e entries)
- Comments
- Empty lines

### Generated Python Package List
```nix
(python3.withPackages (ps: with ps; [
  requests
  flask
  pytest
  black
  pillow
  pyyaml
]))
```

---

## Example 7: Handling Missing Packages

### Scenario
requirements.txt contains: `special-internal-package==1.0.0`

This package doesn't exist in nixpkgs.

### Fallback Strategy
```nix
packages = with pkgs; [
  (python3.withPackages (ps: with ps; [
    # Successfully mapped packages
    requests
    flask
    # Note: special-internal-package not found in nixpkgs
    # Added pip for manual installation
  ]))
  # Adding pip to allow manual package installation
];
```

### User Message
```
Created flake.nix with Python support.

Note: The package 'special-internal-package' was not found in nixpkgs.
You can install it manually using pip after entering the dev shell:
  nix develop
  pip install special-internal-package

Or add it to a local overlay if you need it in the flake.
```

---

## Example 8: Interactive Creation Flow

### User Request
"Initialize a flake for this project"

### Claude's Process
```
1. Detecting languages...
   ✓ Found 5 .scad files → OpenSCAD detected
   ✓ Found 2 .py files → Python detected
   ✓ Found requirements.txt with 3 packages

2. Creating flake.nix with:
   - OpenSCAD (openscad-unstable)
   - Python with packages: qrcode, pillow, requests

3. Would you like me to create .envrc for direnv support? [Yes/No]
   User: Yes

4. Creating .envrc...

5. Complete! Next steps:
   - Run: nix flake update
   - Run: direnv allow
   - Add .direnv/ to .gitignore
   - Commit: git add flake.nix flake.lock .envrc
```

---

## Common Interaction Patterns

### Pattern 1: Basic Request
```
User: "Create a flake.nix for this project"
Claude: [Detects languages, creates flake, provides instructions]
```

### Pattern 2: Flake Already Exists
```
User: "Initialize nix flake"
Claude: "I see flake.nix already exists. Would you like me to:
         1. Update it with detected languages
         2. Skip creation
         3. Create a backup and regenerate"
```

### Pattern 3: No Supported Languages
```
User: "Add flake support"
Claude: [Detects only .txt, .md files - no supported languages]
        "I created a minimal flake.nix with basic shell utilities.
         The project doesn't contain OpenSCAD or Python files.
         Which development tools would you like in the shell?"
```

### Pattern 4: .gitignore Management
```
Claude: "Created flake and direnv files.

         I notice .direnv/ is not in your .gitignore.
         Would you like me to add it? [Yes/No]"
User: "Yes"
Claude: [Adds .direnv/ to .gitignore or creates it if missing]
```
