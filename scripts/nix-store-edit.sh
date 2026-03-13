#!/usr/bin/env bash
# nix-store-edit: Make files symlinked to the nix store editable
#
# This script handles two cases:
# 1. Direct symlink: If the file itself is a symlink to the nix store,
#    replace it with a copy of the file contents
# 2. Nested in symlinked directory: If the file exists inside a directory
#    that is symlinked to the nix store, create a "link farm" - materialize
#    ancestor directories as real directories, keep sibling files as symlinks,
#    but make the target file a real editable copy

set -euo pipefail

usage() {
    echo "Usage: nix-store-edit <filepath>"
    echo ""
    echo "Make a file symlinked to the nix store editable by copying its contents."
    echo ""
    echo "If the file is a direct symlink to /nix/store/..., the symlink is replaced"
    echo "with a copy of the file."
    echo ""
    echo "If the file is inside a directory symlinked to /nix/store/..., a link farm"
    echo "is created: ancestor directories become real directories, sibling files"
    echo "remain as symlinks, and the target file becomes a real editable copy."
    exit 1
}

# Check if a path points to the nix store (directly or through symlinks)
points_to_nix_store() {
    local path="$1"
    local resolved
    resolved=$(realpath "$path" 2>/dev/null) || return 1
    [[ "$resolved" == /nix/store/* ]]
}

# Find the topmost directory component that is a symlink to the nix store
# Returns the symlink path and its target, or empty if none found
find_nix_store_symlink() {
    local path="$1"
    local current=""

    # Normalize the path (make absolute, resolve . and ..)
    path=$(cd "$(dirname "$path")" && pwd)/$(basename "$path")

    # Split path into components and check each prefix
    local IFS='/'
    read -ra components <<< "$path"

    for component in "${components[@]}"; do
        [[ -z "$component" ]] && continue
        current="$current/$component"

        if [[ -L "$current" ]]; then
            local target
            target=$(readlink "$current")
            if [[ "$target" == /nix/store/* ]]; then
                echo "$current"
                echo "$target"
                return 0
            fi
        fi
    done

    return 1
}

# Create a link farm: replace a symlinked directory with a real directory
# containing symlinks to all original contents, then materialize the path
# to the target file
create_link_farm() {
    local symlink_path="$1"
    local original_target="$2"
    local relative_path="$3"  # Path from symlink to target file

    # Create temporary directory for the link farm
    local temp_dir
    temp_dir=$(mktemp -d)

    # Copy the directory structure as symlinks
    # For each item in the original directory, create a symlink
    local item
    for item in "$original_target"/*; do
        [[ -e "$item" ]] || continue
        local name
        name=$(basename "$item")
        ln -s "$item" "$temp_dir/$name"
    done

    # Also handle hidden files
    for item in "$original_target"/.*; do
        local name
        name=$(basename "$item")
        [[ "$name" == "." || "$name" == ".." ]] && continue
        [[ -e "$item" ]] || continue
        ln -s "$item" "$temp_dir/$name"
    done

    # Now we need to materialize the path to our target file
    # Split relative_path and materialize each directory level
    local current_src="$original_target"
    local current_dst="$temp_dir"

    local IFS='/'
    read -ra path_components <<< "$relative_path"

    # Process all but the last component (directories)
    local i
    for ((i=0; i<${#path_components[@]}-1; i++)); do
        local component="${path_components[i]}"
        [[ -z "$component" ]] && continue

        current_src="$current_src/$component"
        current_dst="$current_dst/$component"

        # Remove the symlink if it exists
        if [[ -L "$current_dst" ]]; then
            rm "$current_dst"
        fi

        # Create real directory
        mkdir -p "$current_dst"

        # Populate with symlinks to original contents
        local item
        for item in "$current_src"/*; do
            [[ -e "$item" ]] || continue
            local name
            name=$(basename "$item")
            [[ -e "$current_dst/$name" ]] || ln -s "$item" "$current_dst/$name"
        done

        # Also handle hidden files
        for item in "$current_src"/.*; do
            local name
            name=$(basename "$item")
            [[ "$name" == "." || "$name" == ".." ]] && continue
            [[ -e "$item" ]] || continue
            [[ -e "$current_dst/$name" ]] || ln -s "$item" "$current_dst/$name"
        done
    done

    # Handle the final component (the file itself)
    local filename="${path_components[-1]}"
    local final_src="$current_src/$filename"
    local final_dst="$current_dst/$filename"

    # Remove symlink to the file
    if [[ -L "$final_dst" ]]; then
        rm "$final_dst"
    fi

    # Copy the actual file
    cp -a "$final_src" "$final_dst"

    # Replace the original symlink with our link farm
    rm "$symlink_path"
    mv "$temp_dir" "$symlink_path"

    echo "Created link farm at $symlink_path"
    echo "File $symlink_path/$relative_path is now editable"
}

# Main logic
main() {
    if [[ $# -ne 1 ]]; then
        usage
    fi

    local target_file="$1"

    # Check if the path exists
    if [[ ! -e "$target_file" ]]; then
        echo "Error: File does not exist: $target_file" >&2
        exit 1
    fi

    # Get the absolute path
    local abs_path
    abs_path=$(cd "$(dirname "$target_file")" && pwd)/$(basename "$target_file")

    # Case 1: Symlink to nix store (direct or through intermediate symlinks)
    if [[ -L "$abs_path" ]]; then
        local resolved
        resolved=$(realpath "$abs_path")
        if [[ "$resolved" == /nix/store/* ]]; then
            echo "File is a symlink resolving to nix store"
            # Remove symlink and copy contents
            rm "$abs_path"
            cp -a "$resolved" "$abs_path"
            echo "Replaced symlink with copy: $abs_path"
            exit 0
        fi
    fi

    # Case 2: File inside a directory symlinked to nix store
    # Also check the fully-resolved path, as intermediate symlinks (e.g.
    # /etc/static -> /nix/store/...) may hide nix store directories
    local symlink_info
    if symlink_info=$(find_nix_store_symlink "$abs_path"); then
        local symlink_path symlink_target
        symlink_path=$(echo "$symlink_info" | head -n1)
        symlink_target=$(echo "$symlink_info" | tail -n1)

        # Calculate relative path from symlink to target file
        local relative_path="${abs_path#"$symlink_path"/}"

        echo "File is inside symlinked directory: $symlink_path -> $symlink_target"
        echo "Relative path to file: $relative_path"

        create_link_farm "$symlink_path" "$symlink_target" "$relative_path"
        exit 0
    fi

    # Check if the file resolves to nix store (might be through multiple symlinks)
    if points_to_nix_store "$abs_path"; then
        echo "Error: File resolves to nix store but through an unexpected path structure" >&2
        echo "Target: $abs_path" >&2
        echo "Resolved: $(realpath "$abs_path")" >&2
        exit 1
    fi

    # File is not in the nix store
    echo "File is not symlinked to the nix store: $abs_path"
    echo "Nothing to do."
    exit 0
}

main "$@"
