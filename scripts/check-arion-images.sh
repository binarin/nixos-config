#!/usr/bin/env bash
# Check if arion docker images have newer versions available
#
# Usage: Run with: nix shell nixpkgs#jq nixpkgs#curl -c ./scripts/check-arion-images.sh
#        Or ensure jq and curl are in PATH
set -euo pipefail

# Infrastructure images that don't need frequent updates
# These will be reported in a separate section
INFRA_IMAGES=(
    "meilisearch"
    "postgres"
    "redis"
    "mariadb"
    "mysql"
    "mongo"
    "elasticsearch"
    "memcached"
    "rabbitmq"
    "valkey"
    "sonic"
)

# Check for required commands
for cmd in jq curl; do
    if ! command -v "$cmd" &> /dev/null; then
        echo "Error: $cmd is required but not found in PATH"
        echo "Run with: nix shell nixpkgs#jq nixpkgs#curl -c $0"
        exit 1
    fi
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$REPO_ROOT"

echo "Extracting arion images from all nixosConfigurations..."

# Nix expression to extract all images from arion projects
read -r -d '' NIX_EXPR << 'EOF' || true
let
  flake = builtins.getFlake (toString ./.);
  lib = flake.inputs.nixpkgs.lib;

  # Extract images from a single nixosConfiguration
  extractImages = name: config:
    let
      arionProjects = config.config.virtualisation.arion.projects or {};
      projectImages = lib.mapAttrsToList (projName: proj:
        let
          services = proj.settings.services or {};
          serviceImages = lib.mapAttrsToList (svcName: svc:
            let
              image = svc.service.image or null;
            in
            if image != null then [{
              inherit name image;
              project = projName;
              service = svcName;
            }] else []
          ) services;
        in
        lib.flatten serviceImages
      ) arionProjects;
    in
    lib.flatten projectImages;

  # Extract from all configurations
  allImages = lib.mapAttrsToList extractImages flake.nixosConfigurations;
in
lib.flatten allImages
EOF

# Get all images as JSON
IMAGES_JSON=$(nix eval --json --impure --expr "$NIX_EXPR" 2>/dev/null || echo "[]")

if [[ "$IMAGES_JSON" == "[]" ]]; then
    echo "No arion images found or evaluation failed."
    exit 0
fi

# Parse image reference into components
# Format: [registry/]repository[:tag|@digest]
parse_image() {
    local image="$1"
    local registry="" repo="" tag=""

    # Remove digest if present (we only care about tags)
    image="${image%%@*}"

    # Extract tag
    if [[ "$image" == *:* ]]; then
        tag="${image##*:}"
        image="${image%:*}"
    else
        tag="latest"
    fi

    # Determine registry and repo
    # If the first component contains a dot or colon, it's a registry
    local first_part="${image%%/*}"
    if [[ "$first_part" == *"."* ]] || [[ "$first_part" == *":"* ]] || [[ "$first_part" == "localhost" ]]; then
        registry="$first_part"
        repo="${image#*/}"
    else
        # Docker Hub
        registry="docker.io"
        # Handle library images (single name like "redis", "postgres")
        if [[ "$image" != */* ]]; then
            repo="library/$image"
        else
            repo="$image"
        fi
    fi

    echo "$registry|$repo|$tag"
}

# Get auth token for Docker Hub
get_dockerhub_token() {
    local repo="$1"
    curl -s "https://auth.docker.io/token?service=registry.docker.io&scope=repository:$repo:pull" | jq -r '.token'
}

# Get tags from Docker Hub
get_dockerhub_tags() {
    local repo="$1"
    local token
    token=$(get_dockerhub_token "$repo")
    curl -s -H "Authorization: Bearer $token" \
        "https://registry-1.docker.io/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
}

# Get auth token for GHCR
get_ghcr_token() {
    local repo="$1"
    curl -s "https://ghcr.io/token?scope=repository:$repo:pull" | jq -r '.token' 2>/dev/null || echo ""
}

# Get tags from GHCR (GitHub Container Registry)
get_ghcr_tags() {
    local repo="$1"
    local token
    token=$(get_ghcr_token "$repo")
    if [[ -n "$token" && "$token" != "null" ]]; then
        curl -s -H "Authorization: Bearer $token" \
            "https://ghcr.io/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
    else
        # Fallback without token
        curl -s "https://ghcr.io/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
    fi
}

# Get tags from GCR (Google Container Registry)
get_gcr_tags() {
    local repo="$1"
    curl -s "https://gcr.io/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
}

# Get tags from LinuxServer.io registry (backed by GHCR)
get_lscr_tags() {
    local repo="$1"
    # LSCR is backed by GHCR, so use GHCR token and API
    local token
    token=$(get_ghcr_token "$repo")
    if [[ -n "$token" && "$token" != "null" ]]; then
        curl -s -H "Authorization: Bearer $token" \
            "https://ghcr.io/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
    else
        curl -s "https://ghcr.io/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
    fi
}

# Get tags from any OCI registry
get_generic_tags() {
    local registry="$1"
    local repo="$2"
    curl -s "https://$registry/v2/$repo/tags/list" | jq -r '.tags[]?' 2>/dev/null || echo ""
}

# Get latest release tag from a Gitea instance
get_gitea_latest_release() {
    local registry="$1"
    local repo="$2"
    # Use Gitea API to get latest release
    local release_json
    release_json=$(curl -s "https://$registry/api/v1/repos/$repo/releases/latest" 2>/dev/null)
    if [[ -n "$release_json" ]]; then
        echo "$release_json" | jq -r '.tag_name // empty' 2>/dev/null || echo ""
    fi
}

# Get latest release tag from GitHub
get_github_latest_release() {
    local owner="$1"
    local repo="$2"
    # Use GitHub API to get latest release
    local release_json
    release_json=$(curl -s "https://api.github.com/repos/$owner/$repo/releases/latest" 2>/dev/null)
    if [[ -n "$release_json" ]]; then
        echo "$release_json" | jq -r '.tag_name // empty' 2>/dev/null || echo ""
    fi
}

# Check if an image name matches an infrastructure pattern
is_infra_image() {
    local repo="$1"
    local image_basename="${repo##*/}"  # Get the last component of the path
    image_basename="${image_basename%%:*}"  # Remove any tag

    for pattern in "${INFRA_IMAGES[@]}"; do
        if [[ "$image_basename" == "$pattern" ]] || [[ "$image_basename" == *"$pattern"* ]]; then
            return 0
        fi
    done
    return 1
}

# Get all tags for an image
get_tags() {
    local registry="$1"
    local repo="$2"

    case "$registry" in
        docker.io)
            get_dockerhub_tags "$repo"
            ;;
        ghcr.io)
            get_ghcr_tags "$repo"
            ;;
        gcr.io)
            get_gcr_tags "$repo"
            ;;
        lscr.io)
            get_lscr_tags "$repo"
            ;;
        *)
            get_generic_tags "$registry" "$repo"
            ;;
    esac
}

# Compare version strings (semver-like)
# Returns: -1 if v1 < v2, 0 if v1 == v2, 1 if v1 > v2
compare_versions() {
    local v1="$1"
    local v2="$2"

    # Strip leading 'v' if present
    v1="${v1#v}"
    v2="${v2#v}"

    # Use sort -V for version comparison
    if [[ "$v1" == "$v2" ]]; then
        echo 0
    elif [[ "$(printf '%s\n%s' "$v1" "$v2" | sort -V | head -n1)" == "$v1" ]]; then
        echo -1
    else
        echo 1
    fi
}

# Check if a tag looks like a version (not latest, not a hash, etc.)
is_version_tag() {
    local tag="$1"
    # Match patterns like: 1.2.3, v1.2.3, 1.2, v1.2, 0.29.1, 2.3.6, etc.
    # Also match variants like 16-alpine, v0.5.8
    [[ "$tag" =~ ^v?[0-9]+(\.[0-9]+)*(-[a-zA-Z0-9]+)?$ ]]
}

# Extract the base version pattern from a tag
# e.g., "16-alpine" -> version prefix "16", suffix "-alpine"
get_version_pattern() {
    local tag="$1"
    local version_part suffix_part

    # Strip leading v
    tag="${tag#v}"

    # Split on first dash after version numbers
    if [[ "$tag" =~ ^([0-9]+(\.[0-9]+)*)(-.+)?$ ]]; then
        version_part="${BASH_REMATCH[1]}"
        suffix_part="${BASH_REMATCH[3]:-}"
        echo "$version_part|$suffix_part"
    else
        echo "$tag|"
    fi
}

# Find the latest version among tags that match a pattern
find_latest_matching() {
    local current_tag="$1"
    shift
    local tags=("$@")

    local current_pattern
    current_pattern=$(get_version_pattern "$current_tag")
    local current_version="${current_pattern%%|*}"
    local current_suffix="${current_pattern#*|}"

    local latest_version="$current_version"
    local latest_tag="$current_tag"
    local had_v_prefix=""
    [[ "$current_tag" == v* ]] && had_v_prefix="v"

    for tag in "${tags[@]}"; do
        # Skip non-version tags
        if ! is_version_tag "$tag"; then
            continue
        fi

        local tag_pattern
        tag_pattern=$(get_version_pattern "$tag")
        local tag_version="${tag_pattern%%|*}"
        local tag_suffix="${tag_pattern#*|}"

        # Only compare tags with the same suffix pattern
        if [[ "$tag_suffix" != "$current_suffix" ]]; then
            continue
        fi

        # Check if tag has v prefix
        local tag_had_v=""
        [[ "$tag" == v* ]] && tag_had_v="v"

        # Only compare if v-prefix matches
        if [[ "$had_v_prefix" != "$tag_had_v" ]]; then
            continue
        fi

        local cmp
        cmp=$(compare_versions "$tag_version" "$latest_version")
        if [[ "$cmp" -gt 0 ]]; then
            latest_version="$tag_version"
            latest_tag="$tag"
        fi
    done

    echo "$latest_tag"
}

# Find the latest versioned tag (for :latest images that need pinning)
# Prefers v-prefixed semver tags, then plain semver, then any version-like tag
find_latest_version() {
    local tags=("$@")
    local best_tag=""
    local best_version=""
    local best_score=0  # Higher is better: 3=v-prefixed semver, 2=plain semver, 1=other version

    for tag in "${tags[@]}"; do
        # Skip non-version tags
        if ! is_version_tag "$tag"; then
            continue
        fi

        local tag_pattern
        tag_pattern=$(get_version_pattern "$tag")
        local tag_version="${tag_pattern%%|*}"
        local tag_suffix="${tag_pattern#*|}"

        # Skip tags with suffixes for cleaner recommendations (prefer "1.2.3" over "1.2.3-alpine")
        # unless we haven't found anything yet
        if [[ -n "$tag_suffix" && -n "$best_tag" && -z "$(get_version_pattern "$best_tag" | cut -d'|' -f2)" ]]; then
            continue
        fi

        # Calculate score
        local score=1
        if [[ "$tag" == v* ]]; then
            # v-prefixed versions (v1.2.3)
            if [[ "$tag_version" == *.*.* ]]; then
                score=3  # v-prefixed semver
            else
                score=2
            fi
        else
            # Non-v-prefixed
            if [[ "$tag_version" == *.*.* ]]; then
                score=2  # Plain semver
            else
                score=1
            fi
        fi

        # If this tag has a better score, or same score but higher version
        if [[ $score -gt $best_score ]]; then
            best_score=$score
            best_version="$tag_version"
            best_tag="$tag"
        elif [[ $score -eq $best_score ]]; then
            local cmp
            cmp=$(compare_versions "$tag_version" "$best_version")
            if [[ "$cmp" -gt 0 ]]; then
                best_version="$tag_version"
                best_tag="$tag"
            fi
        fi
    done

    echo "$best_tag"
}

echo ""
echo "Checking for updates..."
echo ""

# Track statistics
updates_found=0
infra_updates_found=0
up_to_date=0
errors=0
skipped=0
unpinned=0

# Arrays to collect output for different sections
declare -a regular_updates=()
declare -a infra_updates=()
declare -a ok_messages=()
declare -a pin_messages=()
declare -a skip_messages=()
declare -a error_messages=()

# Process each image (using process substitution to avoid subshell variable scoping issue)
while read -r entry; do
    machine=$(echo "$entry" | jq -r '.name')
    project=$(echo "$entry" | jq -r '.project')
    service=$(echo "$entry" | jq -r '.service')
    image=$(echo "$entry" | jq -r '.image')

    # Parse the image
    parsed=$(parse_image "$image")
    registry="${parsed%%|*}"
    rest="${parsed#*|}"
    repo="${rest%%|*}"
    tag="${rest#*|}"

    # Special handling for Gitea registries - try to get latest release
    if [[ "$registry" == *"gitea"* ]] || [[ "$registry" == *".baerentsen.space"* ]]; then
        gitea_latest=$(get_gitea_latest_release "$registry" "$repo")
        if [[ -n "$gitea_latest" ]]; then
            # Strip leading v for comparison if present in one but not other
            current_clean="${tag#v}"
            latest_clean="${gitea_latest#v}"
            if [[ "$current_clean" == "$latest_clean" ]]; then
                ok_messages+=("${GREEN}OK${NC} $machine/$project/$service: $image")
                ((up_to_date++)) || true
            else
                if is_infra_image "$repo"; then
                    infra_updates+=("${RED}UPDATE${NC} $machine/$project/$service: $image -> $gitea_latest")
                    ((infra_updates_found++)) || true
                else
                    regular_updates+=("${RED}UPDATE${NC} $machine/$project/$service: $image -> $gitea_latest")
                    ((updates_found++)) || true
                fi
            fi
            continue
        fi
        # Fall through to regular tag checking if Gitea API didn't work
    fi

    # Special handling for LinuxServer.io images - check GitHub releases
    if [[ "$registry" == "lscr.io" ]] && [[ "$repo" == linuxserver/* ]]; then
        image_name="${repo#linuxserver/}"
        github_latest=$(get_github_latest_release "linuxserver" "docker-${image_name}")
        if [[ -n "$github_latest" ]]; then
            # Strip leading v for comparison if present in one but not other
            current_clean="${tag#v}"
            latest_clean="${github_latest#v}"
            if [[ "$current_clean" == "$latest_clean" ]]; then
                ok_messages+=("${GREEN}OK${NC} $machine/$project/$service: $image")
                ((up_to_date++)) || true
            else
                if is_infra_image "$repo"; then
                    infra_updates+=("${RED}UPDATE${NC} $machine/$project/$service: $image -> $github_latest")
                    ((infra_updates_found++)) || true
                else
                    regular_updates+=("${RED}UPDATE${NC} $machine/$project/$service: $image -> $github_latest")
                    ((updates_found++)) || true
                fi
            fi
            continue
        fi
        # Fall through to regular tag checking if GitHub API didn't work
    fi

    # Get available tags
    tags_raw=$(get_tags "$registry" "$repo" 2>/dev/null)
    if [[ -z "$tags_raw" ]]; then
        error_messages+=("${RED}ERROR${NC} $machine/$project/$service: $image (failed to fetch tags from $registry)")
        ((errors++)) || true
        continue
    fi

    # Convert to array
    mapfile -t tags <<< "$tags_raw"

    # Handle 'latest' tags - find best version to pin to
    if [[ "$tag" == "latest" ]]; then
        latest=$(find_latest_version "${tags[@]}")
        if [[ -n "$latest" ]]; then
            pin_messages+=("${YELLOW}PIN${NC} $machine/$project/$service: $image -> $latest")
            ((unpinned++)) || true
        else
            skip_messages+=("${YELLOW}SKIP${NC} $machine/$project/$service: $image (no versioned tags found)")
            ((skipped++)) || true
        fi
        continue
    fi

    # Skip non-version tags
    if ! is_version_tag "$tag"; then
        skip_messages+=("${YELLOW}SKIP${NC} $machine/$project/$service: $image (non-version tag)")
        ((skipped++)) || true
        continue
    fi

    # Find latest matching version
    latest=$(find_latest_matching "$tag" "${tags[@]}")

    if [[ "$latest" == "$tag" ]]; then
        ok_messages+=("${GREEN}OK${NC} $machine/$project/$service: $image")
        ((up_to_date++)) || true
    else
        if is_infra_image "$repo"; then
            infra_updates+=("${RED}UPDATE${NC} $machine/$project/$service: $image -> $latest")
            ((infra_updates_found++)) || true
        else
            regular_updates+=("${RED}UPDATE${NC} $machine/$project/$service: $image -> $latest")
            ((updates_found++)) || true
        fi
    fi
done < <(echo "$IMAGES_JSON" | jq -c '.[]')

# Output results in sections
if [[ ${#regular_updates[@]} -gt 0 ]]; then
    echo "========================"
    echo "Updates Available"
    echo "========================"
    for msg in "${regular_updates[@]}"; do
        echo -e "$msg"
    done
    echo ""
fi

if [[ ${#infra_updates[@]} -gt 0 ]]; then
    echo "========================"
    echo "Infrastructure Updates (low priority)"
    echo "========================"
    for msg in "${infra_updates[@]}"; do
        echo -e "$msg"
    done
    echo ""
fi

if [[ ${#pin_messages[@]} -gt 0 ]]; then
    echo "========================"
    echo "Unpinned Images"
    echo "========================"
    for msg in "${pin_messages[@]}"; do
        echo -e "$msg"
    done
    echo ""
fi

if [[ ${#ok_messages[@]} -gt 0 ]]; then
    echo "========================"
    echo "Up to Date"
    echo "========================"
    for msg in "${ok_messages[@]}"; do
        echo -e "$msg"
    done
    echo ""
fi

if [[ ${#skip_messages[@]} -gt 0 ]]; then
    echo "========================"
    echo "Skipped"
    echo "========================"
    for msg in "${skip_messages[@]}"; do
        echo -e "$msg"
    done
    echo ""
fi

if [[ ${#error_messages[@]} -gt 0 ]]; then
    echo "========================"
    echo "Errors"
    echo "========================"
    for msg in "${error_messages[@]}"; do
        echo -e "$msg"
    done
    echo ""
fi

echo "========================"
echo "Summary:"
echo "  Up to date: $up_to_date"
echo "  Updates available: $updates_found"
echo "  Infrastructure updates: $infra_updates_found"
echo "  Unpinned (latest): $unpinned"
echo "  Skipped: $skipped"
echo "  Errors: $errors"
