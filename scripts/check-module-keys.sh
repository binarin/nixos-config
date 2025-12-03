#!/usr/bin/env bash
set -euo pipefail

# Check that every module in modules/ that defines nixosModules or homeModules has a proper key

MODULES_DIR="modules"
EXIT_CODE=0
declare -a ERRORS

while IFS= read -r file; do
    # Find all module definitions in the file
    # Look for: flake.nixosModules.foo, flake.homeModules.bar, flake.modules.generic.baz

    # Extract nixosModules
    while IFS= read -r module_name; do
        expected_key="nixos-config.modules.nixos.$module_name"
        if ! grep -q "key = \"$expected_key\"" "$file"; then
            ERRORS+=("$file: flake.nixosModules.$module_name missing key = \"$expected_key\"")
            EXIT_CODE=1
        fi
    done < <(grep -oP 'flake\.nixosModules\.\K[a-zA-Z0-9_-]+' "$file" | sort -u)

    # Extract homeModules
    while IFS= read -r module_name; do
        expected_key="nixos-config.modules.home.$module_name"
        if ! grep -q "key = \"$expected_key\"" "$file"; then
            ERRORS+=("$file: flake.homeModules.$module_name missing key = \"$expected_key\"")
            EXIT_CODE=1
        fi
    done < <(grep -oP 'flake\.homeModules\.\K[a-zA-Z0-9_-]+' "$file" | sort -u)

    # Extract generic modules
    while IFS= read -r module_name; do
        expected_key="nixos-config.modules.generic.$module_name"
        if ! grep -q "key = \"$expected_key\"" "$file"; then
            ERRORS+=("$file: flake.modules.generic.$module_name missing key = \"$expected_key\"")
            EXIT_CODE=1
        fi
    done < <(grep -oP 'flake\.modules\.generic\.\K[a-zA-Z0-9_-]+' "$file" | sort -u)

done < <(find "$MODULES_DIR" -name "*.nix" -type f)

if [ $EXIT_CODE -ne 0 ]; then
    echo "ERROR: The following modules are missing proper 'key' attributes:" >&2
    printf '  - %s\n' "${ERRORS[@]}" >&2
    echo >&2
    echo "According to CLAUDE.md, having 'key' in every module is of utmost importance," >&2
    echo "as it's used for deduplication by the module system." >&2
    exit 1
else
    echo "✓ All modules have proper key attributes"
fi
