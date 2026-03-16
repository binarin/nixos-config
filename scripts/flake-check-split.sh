#!/usr/bin/env bash
# Split replacement for `nix flake check` that runs each check in its own
# nix eval/build process (separate evaluator → separate heap → bounded memory).
# Faithfully replicates what the C++ `nix flake check` does, but each item
# gets its own process.
#
# And in CI, almost everything, except for nixosModules, is already
# handled by dynamic matrix

set -uo pipefail

FLAKE="${1:-.}"
SYSTEM="${NIX_SYSTEM:-$(nix eval --raw --impure --expr builtins.currentSystem)}"
ALL_SYSTEMS="${ALL_SYSTEMS:-}"  # set to 1 to check all systems
JOBS="${JOBS:-4}"               # max parallel nix eval processes

TMPDIR_ROOT=$(mktemp -d)
trap 'rm -rf "$TMPDIR_ROOT"' EXIT

FAIL_DIR="$TMPDIR_ROOT/fail"
WARN_DIR="$TMPDIR_ROOT/warn"
mkdir -p "$FAIL_DIR" "$WARN_DIR"

# ── helpers ──────────────────────────────────────────────────────────────────

log()  { echo "  [check] $*" >&2; }

record_warn() {
    echo "$1" > "$WARN_DIR/$(date +%s%N)-$$-$RANDOM"
}

record_fail() {
    echo "$1" > "$FAIL_DIR/$(date +%s%N)-$$-$RANDOM"
}

# List attribute names at a flake path (returns newline-separated names).
list_attrs() {
    nix eval --json "$FLAKE#$1" --apply 'x: builtins.attrNames x' 2>/dev/null \
        | jq -r '.[]'
}

valid_system() {
    [[ "$1" == *-* ]]
}

should_check_system() {
    if [[ -n "$ALL_SYSTEMS" ]]; then
        return 0
    fi
    if [[ "$1" != "$SYSTEM" ]]; then
        echo "$1" >> "$TMPDIR_ROOT/omitted_systems"
        return 1
    fi
}

# ── per-output-type checks ───────────────────────────────────────────────────

# checkDerivation: force-eval as derivation, return drvPath
check_derivation() {
    local attr_path="$1"
    log "derivation $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path" --json \
        --apply 'drv: if drv ? type && drv.type == "derivation" then drv.drvPath else throw "not a derivation"' \
        2>&1); then
        record_fail "$attr_path: $output"
        return 1
    fi
    echo "$output"  # quoted drvPath for caller
}

# build_check: eval + build a checks.* derivation
build_check() {
    local attr_path="$1"
    log "build $attr_path"
    local output
    if ! output=$(nix build "$FLAKE#$attr_path" --no-link 2>&1); then
        record_fail "$attr_path: $output"
        return 1
    fi
}

# checks.<system>.<name> — must be derivations, and get built
check_checks() {
    local systems
    systems=$(list_attrs "checks") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "checks.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        local names
        names=$(list_attrs "checks.$sys") || continue
        for name in $names; do
            build_check "checks.$sys.$name"
        done
    done
}

# formatter.<system> — must be a derivation (not built)
check_formatter() {
    local systems
    systems=$(list_attrs "formatter") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "formatter.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        check_derivation "formatter.$sys" > /dev/null
    done
}

# packages.<system>.<name> and devShells.<system>.<name> — must be derivations
check_per_system_derivations() {
    local output_name="$1"
    local systems
    systems=$(list_attrs "$output_name") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "$output_name.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        local names
        names=$(list_attrs "$output_name.$sys") || continue
        for name in $names; do
            check_derivation "$output_name.$sys.$name" > /dev/null
        done
    done
}

# apps.<system>.<name> — must have type, program; no extra attrs
check_app() {
    local attr_path="$1"
    log "app $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path" --json \
        --apply '
          app:
            let
              has = n: builtins.hasAttr n app;
              extra = builtins.removeAttrs app ["type" "program" "meta"];
            in
              if !(has "type")    then throw "app lacks attribute type"
              else if !(has "program") then throw "app lacks attribute program"
              else if extra != {} then throw "app has unsupported attributes: ${builtins.concatStringsSep ", " (builtins.attrNames extra)}"
              else "ok"
        ' 2>&1); then
        record_fail "$attr_path: $output"
    fi
}

check_apps() {
    local systems
    systems=$(list_attrs "apps") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "apps.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        local names
        names=$(list_attrs "apps.$sys") || continue
        for name in $names; do
            check_app "apps.$sys.$name"
        done
    done
}

# defaultPackage.<system>, devShell.<system> — deprecated per-system derivation
check_deprecated_per_system_derivation() {
    local name="$1" replacement="$2"
    record_warn "flake output '$name' is deprecated; use '$replacement' instead"
    local systems
    systems=$(list_attrs "$name") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "$name.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        check_derivation "$name.$sys" > /dev/null
    done
}

# defaultApp.<system> — deprecated per-system app
check_deprecated_per_system_app() {
    local name="$1" replacement="$2"
    record_warn "flake output '$name' is deprecated; use '$replacement' instead"
    local systems
    systems=$(list_attrs "$name") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "$name.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        check_app "$name.$sys"
    done
}

# legacyPackages.<system> — only check system names, no deep eval
check_legacy_packages() {
    local systems
    systems=$(list_attrs "legacyPackages") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "legacyPackages.$sys: invalid system name"; continue; }
        should_check_system "$sys" || true
    done
}

# overlays.<name> — must be a function (C++ also checks arg named "final", can't do from CLI)
check_overlay() {
    local attr_path="$1"
    log "overlay $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path" \
        --apply 'f: if builtins.isFunction f then "ok" else throw "overlay is not a function"' \
        2>&1); then
        record_fail "$attr_path: $output"
    fi
}

check_overlays() {
    local names
    names=$(list_attrs "overlays") || return 0
    for name in $names; do
        check_overlay "overlays.$name"
    done
}

# nixosModules.<name> — forceValue (just eval it)
check_module() {
    local attr_path="$1"
    log "module $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path" \
        --apply 'x: builtins.seq x "ok"' \
        2>&1); then
        record_fail "$attr_path: $output"
    fi
}

check_nixos_modules() {
    local names
    names=$(list_attrs "nixosModules") || return 0
    for name in $names; do
        check_module "nixosModules.$name"
    done
}

# nixosConfigurations.<name> — must have config.system.build.toplevel as derivation
check_nixos_configuration() {
    local attr_path="$1"
    log "nixosConfiguration $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path.config.system.build.toplevel" --json \
        --apply 'drv: if drv ? type && drv.type == "derivation" then drv.drvPath else throw "config.system.build.toplevel is not a derivation"' \
        2>&1); then
        record_fail "$attr_path: $output"
    fi
}

check_nixos_configurations() {
    local names
    names=$(list_attrs "nixosConfigurations") || return 0
    for name in $names; do
        check_nixos_configuration "nixosConfigurations.$name"
    done
}

# hydraJobs — recursive: leaves must be derivations, top-level must not be
check_hydra_jobs() {
    local paths
    paths=$(nix eval --json "$FLAKE#hydraJobs" --apply '
      let
        collect = prefix: v:
          if v ? type && v.type == "derivation"
          then [prefix]
          else if builtins.isAttrs v
          then builtins.concatLists (
            builtins.map (name:
              collect (if prefix == "" then name else "${prefix}.${name}") v.${name}
            ) (builtins.attrNames v)
          )
          else [];
      in collect ""
    ' 2>/dev/null) || return 0

    echo "$paths" | jq -r '.[]' | while read -r p; do
        check_derivation "hydraJobs.$p" > /dev/null
    done
}

# templates.<name> — must have path (existing) + description, optional welcomeText
check_template() {
    local attr_path="$1"
    log "template $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path" --json \
        --apply '
          t:
            let
              has = n: builtins.hasAttr n t;
              extra = builtins.removeAttrs t ["path" "description" "welcomeText"];
            in
              if !(has "path")        then throw "template lacks attribute path"
              else if !(has "description") then throw "template lacks attribute description"
              else if extra != {}     then throw "template has unsupported attributes: ${builtins.concatStringsSep ", " (builtins.attrNames extra)}"
              else "ok"
        ' 2>&1); then
        record_fail "$attr_path: $output"
    fi
}

check_templates() {
    local names
    names=$(list_attrs "templates") || return 0
    for name in $names; do
        check_template "templates.$name"
    done
}

# bundlers.<system>.<name> — must be a function
check_bundler() {
    local attr_path="$1"
    log "bundler $attr_path"
    local output
    if ! output=$(nix eval "$FLAKE#$attr_path" \
        --apply 'f: if builtins.isFunction f then "ok" else throw "bundler is not a function"' \
        2>&1); then
        record_fail "$attr_path: $output"
    fi
}

check_bundlers() {
    local systems
    systems=$(list_attrs "bundlers") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "bundlers.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        local names
        names=$(list_attrs "bundlers.$sys") || continue
        for name in $names; do
            check_bundler "bundlers.$sys.$name"
        done
    done
}

# defaultBundler.<system> — deprecated
check_default_bundler() {
    record_warn "output 'defaultBundler' is deprecated; use 'bundlers.<system>.default'"
    local systems
    systems=$(list_attrs "defaultBundler") || return 0
    for sys in $systems; do
        valid_system "$sys" || { record_fail "defaultBundler.$sys: invalid system name"; continue; }
        should_check_system "$sys" || continue
        check_bundler "defaultBundler.$sys"
    done
}

# ── parallel runner ──────────────────────────────────────────────────────────

job_fifo="$TMPDIR_ROOT/jobs.fifo"
mkfifo "$job_fifo"
exec 3<>"$job_fifo"
for ((i=0; i<JOBS; i++)); do echo >&3; done

bg_pids=()

run_parallel() {
    read -u 3  # acquire a slot
    {
        "$@"
        echo >&3  # release slot
    } &
    bg_pids+=($!)
}

wait_all() {
    local rc=0
    for pid in "${bg_pids[@]+"${bg_pids[@]}"}"; do
        wait "$pid" || rc=1
    done
    bg_pids=()
    return $rc
}

# ── main ─────────────────────────────────────────────────────────────────────

echo "Checking flake at $FLAKE (system: $SYSTEM)"
echo ""

# Discover which outputs the flake has.
all_outputs=$(nix flake show "$FLAKE" --json 2>/dev/null | jq -r 'keys[]')

# Known-but-unchecked (nix flake check ignores these too).
declare -A KNOWN_UNCHECKED=(
    [lib]=1 [darwinConfigurations]=1 [darwinModules]=1
    [flakeModule]=1 [flakeModules]=1 [herculesCI]=1
    [homeConfigurations]=1 [homeModule]=1 [homeModules]=1
    [nixopsConfigurations]=1
)

for output in $all_outputs; do
    [[ -n "${KNOWN_UNCHECKED[$output]:-}" ]] && continue

    case "$output" in
        checks)             run_parallel check_checks ;;
        formatter)          run_parallel check_formatter ;;
        packages)           run_parallel check_per_system_derivations packages ;;
        devShells)          run_parallel check_per_system_derivations devShells ;;
        apps)               run_parallel check_apps ;;
        legacyPackages)     run_parallel check_legacy_packages ;;
        overlays)           run_parallel check_overlays ;;
        nixosModules)       run_parallel check_nixos_modules ;;
        nixosConfigurations) run_parallel check_nixos_configurations ;;
        hydraJobs)          run_parallel check_hydra_jobs ;;
        templates)          run_parallel check_templates ;;
        bundlers)           run_parallel check_bundlers ;;

        # deprecated singular forms
        overlay)         run_parallel check_overlay "overlay"; record_warn "output 'overlay' is deprecated; use 'overlays.default'" ;;
        nixosModule)     run_parallel check_module "nixosModule"; record_warn "output 'nixosModule' is deprecated; use 'nixosModules.default'" ;;
        defaultPackage)  run_parallel check_deprecated_per_system_derivation "defaultPackage" "packages.<system>.default" ;;
        devShell)        run_parallel check_deprecated_per_system_derivation "devShell" "devShells.<system>.default" ;;
        defaultApp)      run_parallel check_deprecated_per_system_app "defaultApp" "apps.<system>.default" ;;
        defaultTemplate) run_parallel check_template "defaultTemplate"; record_warn "output 'defaultTemplate' is deprecated; use 'templates.default'" ;;
        defaultBundler)  run_parallel check_default_bundler ;;
        *)
            record_warn "unknown flake output '$output'"
            ;;
    esac
done

wait_all || true
exec 3>&-

echo ""

# Collect results from filesystem.
warnings=()
while IFS= read -r -d '' f; do
    warnings+=("$(cat "$f")")
done < <(find "$WARN_DIR" -type f -print0 2>/dev/null)

errors=()
while IFS= read -r -d '' f; do
    errors+=("$(cat "$f")")
done < <(find "$FAIL_DIR" -type f -print0 2>/dev/null)

omitted=()
if [[ -f "$TMPDIR_ROOT/omitted_systems" ]]; then
    while IFS= read -r s; do
        omitted+=("$s")
    done < <(sort -u "$TMPDIR_ROOT/omitted_systems")
fi

if [[ ${#warnings[@]} -gt 0 ]]; then
    echo "Warnings:"
    printf '  - %s\n' "${warnings[@]}"
    echo ""
fi

if [[ ${#errors[@]} -gt 0 ]]; then
    echo "ERRORS:"
    printf '  - %s\n' "${errors[@]}"
    exit 1
fi

echo "all checks passed!"

if [[ ${#omitted[@]} -gt 0 ]]; then
    echo ""
    echo "The check omitted these incompatible systems: ${omitted[*]}"
    echo "Use ALL_SYSTEMS=1 to check all."
fi
