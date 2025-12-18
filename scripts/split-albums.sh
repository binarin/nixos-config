#!/usr/bin/env bash
set -euo pipefail

# Configuration
SUPPORTED_FORMATS=(ape flac)
OUTPUT_BASE="/media/music/import"
DISC_PATTERNS=("Disc " "CD" "Disk ")

# Command-line options
FILTER_REGEX=""
INCLUDE_REGEX=""
SUBDIRS=()

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# Statistics
declare -A album_stats
declare -A album_releases
total_tracks=0
processed_albums=0
failed_albums=0
skipped_albums=0

# Usage information
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Split album image files (APE/FLAC) into individual tracks using CUE sheets.

Options:
    -i, --include REGEX     Only process albums matching this regex pattern
    -f, --filter REGEX      Exclude albums matching this regex pattern
    -s, --subdir DIR        Look for albums in subdirectory DIR (can be specified multiple times)
                           e.g., -s Albums -s EPs to check both Albums/ and EPs/ subdirectories
    -o, --output PATH       Output base directory (default: /media/music/import)
    -h, --help             Show this help message

Examples:
    # Process all albums in current directory
    $(basename "$0")

    # Only process albums from 2000s
    $(basename "$0") --include '^200[0-9]'

    # Exclude albums with 'Live' in the name
    $(basename "$0") --filter 'Live'

    # Only 2000s albums, but exclude Live and Remaster
    $(basename "$0") -i '^200[0-9]' -f 'Live|Remaster'

    # Process albums in Albums/ and EPs/ subdirectories
    $(basename "$0") --subdir Albums --subdir EPs

EOF
    exit 0
}

# Parse command-line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -i|--include)
                INCLUDE_REGEX="$2"
                shift 2
                ;;
            -f|--filter)
                FILTER_REGEX="$2"
                shift 2
                ;;
            -s|--subdir)
                SUBDIRS+=("$2")
                shift 2
                ;;
            -o|--output)
                OUTPUT_BASE="$2"
                shift 2
                ;;
            -h|--help)
                usage
                ;;
            *)
                echo -e "${RED}Error: Unknown option '$1'${NC}" >&2
                echo "Use --help for usage information" >&2
                exit 1
                ;;
        esac
    done
}

# Function to detect encoding of a file
detect_encoding() {
    local file="$1"
    local detected=""

    # Try different methods to detect encoding
    if command -v chardetect &> /dev/null; then
        detected=$(chardetect "$file" 2>/dev/null | awk '{print $2}')
    fi

    if [[ -z "$detected" || "$detected" == "-" ]] && command -v file &> /dev/null; then
        detected=$(file -bi "$file" | sed -n 's/.*charset=\(.*\)/\1/p')
    fi

    # Validate and normalize the detected encoding
    if [[ -z "$detected" || "$detected" == "-" || "$detected" == "binary" || "$detected" == "us-ascii" || "$detected" == "unknown-8bit" ]]; then
        # Default to common Russian encoding for Cyrillic content
        echo "cp1251"
    else
        echo "$detected"
    fi
}

# Function to find audio file in directory
find_audio_file() {
    local dir="$1"

    for format in "${SUPPORTED_FORMATS[@]}"; do
        local files=("$dir"/*."$format")
        if [[ -e "${files[0]}" ]]; then
            echo "${files[0]}"
            return 0
        fi
    done

    return 1
}

# Function to get output format from input file
get_output_format() {
    local input_file="$1"
    local ext="${input_file##*.}"

    # Map extensions to shnsplit output formats
    case "$ext" in
        ape) echo "ape" ;;
        flac) echo "flac" ;;
        *) echo "wav" ;; # fallback
    esac
}

# Function to check if a directory name matches disc patterns
is_disc_dir() {
    local dirname="$1"

    for pattern in "${DISC_PATTERNS[@]}"; do
        if [[ "$dirname" =~ ^${pattern}[0-9]+$ ]]; then
            return 0
        fi
    done

    return 1
}

# Function to find disc subdirectories
find_disc_dirs() {
    local dir="$1"
    local disc_dirs=()

    while IFS= read -r -d '' subdir; do
        local subdir_name=$(basename "$subdir")
        if is_disc_dir "$subdir_name"; then
            disc_dirs+=("$subdir")
        fi
    done < <(find "$dir" -mindepth 1 -maxdepth 1 -type d -print0 | sort -z)

    if [[ ${#disc_dirs[@]} -gt 0 ]]; then
        printf '%s\0' "${disc_dirs[@]}"
        return 0
    fi

    return 1
}

# Function to check if directory has audio content (either direct files or disc subdirs)
has_audio_content() {
    local dir="$1"

    # Check if it has audio files directly
    if find_audio_file "$dir" &>/dev/null; then
        return 0
    fi

    # Check if it has disc subdirectories with audio files
    if find_disc_dirs "$dir" &>/dev/null; then
        return 0
    fi

    return 1
}

# Function to find the earliest release subdirectory
find_earliest_release() {
    local dir="$1"
    local -a candidates
    local release_dir release_name

    # Get all subdirectories, sorted
    mapfile -t candidates < <(find "$dir" -mindepth 1 -maxdepth 1 -type d ! -name "." | sort)

    # Process directories in sorted order to get the earliest
    for release_dir in "${candidates[@]}"; do
        release_name=$(basename "$release_dir")

        # Skip "Scans Only" directories
        if [[ "$release_name" =~ "Scans Only" ]]; then
            continue
        fi

        # Skip disc directories (these will be handled separately)
        if is_disc_dir "$release_name"; then
            continue
        fi

        # Check if this subdirectory contains audio files or disc subdirectories
        if has_audio_content "$release_dir"; then
            # Return the first matching release (earliest)
            echo "$release_dir"
            return 0
        fi
    done

    return 1
}

# Function to process a single album
process_album() {
    local album_dir="$1"
    local album_name="$2"
    local artist_name="$3"
    local current="$4"
    local total="$5"

    echo -e "${CYAN}[${current}/${total}]${NC} ${BOLD}Processing:${NC} ${album_name}"

    # Check if this directory contains release subdirectories
    local actual_dir="$album_dir"
    local earliest_release
    local selected_release=""
    if earliest_release=$(find_earliest_release "$album_dir"); then
        local release_name=$(basename "$earliest_release")
        echo -e "${RED}  ⚠ Multiple releases found, automatically picked earliest: ${release_name}${NC}"
        actual_dir="$earliest_release"
        selected_release="$release_name"
    fi

    # Check if this directory contains disc subdirectories
    local disc_dirs=()
    while IFS= read -r -d '' disc_dir; do
        disc_dirs+=("$disc_dir")
    done < <(find_disc_dirs "$actual_dir" 2>/dev/null || true)

    # Create target directory
    local target_dir="$OUTPUT_BASE/$artist_name/$album_name"
    mkdir -p "$target_dir"
    echo -e "${BLUE}  • Target:${NC} $target_dir"

    if [[ ${#disc_dirs[@]} -gt 0 ]]; then
        # Multi-disc album
        echo -e "${YELLOW}  • Multi-disc album detected: ${#disc_dirs[@]} discs${NC}"

        local total_disc_tracks=0
        for disc_dir in "${disc_dirs[@]}"; do
            local disc_name=$(basename "$disc_dir")
            echo -e "${YELLOW}    ⚙ Processing ${disc_name}...${NC}"

            # Find audio file in disc directory
            local audio_file
            if ! audio_file=$(find_audio_file "$disc_dir"); then
                echo -e "${RED}      ✗ No supported audio file found in ${disc_name}${NC}"
                continue
            fi

            echo -e "${BLUE}      • Audio file:${NC} $(basename "$audio_file")"

            # Find CUE file in disc directory
            local cue_files=("$disc_dir"/*.cue)
            if [[ ! -e "${cue_files[0]}" ]]; then
                echo -e "${RED}      ✗ No CUE file found in ${disc_name}${NC}"
                continue
            fi

            local cue_file="${cue_files[0]}"
            echo -e "${BLUE}      • CUE file:${NC} $(basename "$cue_file")"

            # Detect encoding
            local encoding=$(detect_encoding "$cue_file")
            echo -e "${BLUE}      • Encoding:${NC} ${encoding}"

            # Determine output format
            local output_format=$(get_output_format "$audio_file")

            # Split the disc
            shnsplit -f <(iconv -f "$encoding" "$cue_file") \
                     -o "$output_format" \
                     -O always \
                     -d "$target_dir" \
                     -t "${disc_name} - %n - %t" \
                     "$audio_file" > /dev/null 2>&1

            # Count only tracks from this specific disc
            local disc_tracks
            disc_tracks=$(find "$target_dir" -type f -name "${disc_name} - *.${output_format}" 2>/dev/null | wc -l)

            total_disc_tracks=$((total_disc_tracks + disc_tracks))
            echo -e "${GREEN}      ✓ ${disc_tracks} tracks extracted from ${disc_name}${NC}"
        done

        if [[ $total_disc_tracks -gt 0 ]]; then
            album_stats["$album_name"]=$total_disc_tracks
            if [[ -n "$selected_release" ]]; then
                album_releases["$album_name"]="$selected_release"
            fi
            total_tracks=$((total_tracks + total_disc_tracks))
            processed_albums=$((processed_albums + 1))
            echo -e "${GREEN}  ✓ Success: ${total_disc_tracks} tracks extracted from ${#disc_dirs[@]} discs${NC}"
            return 0
        else
            echo -e "${RED}  ✗ No tracks extracted from any disc${NC}"
            failed_albums=$((failed_albums + 1))
            return 1
        fi
    else
        # Single disc album
        # Find audio file
        local audio_file
        if ! audio_file=$(find_audio_file "$actual_dir"); then
            echo -e "${RED}  ✗ No supported audio file found${NC}"
            failed_albums=$((failed_albums + 1))
            return 1
        fi

        echo -e "${BLUE}  • Audio file:${NC} $(basename "$audio_file")"

        # Find CUE file in the actual directory
        local cue_files=("$actual_dir"/*.cue)
        if [[ ! -e "${cue_files[0]}" ]]; then
            echo -e "${RED}  ✗ No CUE file found${NC}"
            failed_albums=$((failed_albums + 1))
            return 1
        fi

        local cue_file="${cue_files[0]}"
        echo -e "${BLUE}  • CUE file:${NC} $(basename "$cue_file")"

        # Detect encoding
        local encoding=$(detect_encoding "$cue_file")
        echo -e "${BLUE}  • Encoding:${NC} ${encoding}"

        # Determine output format
        local output_format=$(get_output_format "$audio_file")

        # Split the file
        echo -e "${YELLOW}  ⚙ Splitting tracks...${NC}"

        local split_output
        if split_output=$(shnsplit -f <(iconv -f "$encoding" "$cue_file") \
                          -o "$output_format" \
                          -O always \
                          -d "$target_dir" \
                          -t "%n - %t" \
                          "$audio_file" 2>&1); then

            # Count tracks created
            local track_count=$(find "$target_dir" -type f -name "*.${output_format}" 2>/dev/null | wc -l)
            album_stats["$album_name"]=$track_count
            if [[ -n "$selected_release" ]]; then
                album_releases["$album_name"]="$selected_release"
            fi
            total_tracks=$((total_tracks + track_count))
            processed_albums=$((processed_albums + 1))

            echo -e "${GREEN}  ✓ Success: ${track_count} tracks extracted${NC}"
            return 0
        else
            echo -e "${RED}  ✗ Failed to split${NC}"
            echo "$split_output" | sed 's/^/    /'
            failed_albums=$((failed_albums + 1))
            return 1
        fi
    fi
}

# Main script
main() {
    # Get artist name from current directory
    local artist_name
    artist_name="$(basename "$(pwd)")"

    echo -e "${BOLD}${MAGENTA}════════════════════════════════════════${NC}"
    echo -e "${BOLD}${MAGENTA}  Album Splitter${NC}"
    echo -e "${BOLD}${MAGENTA}════════════════════════════════════════${NC}"
    echo -e "${BOLD}Artist:${NC} $artist_name"

    # Show include filter if set
    if [[ -n "$INCLUDE_REGEX" ]]; then
        echo -e "${BOLD}Include:${NC} $INCLUDE_REGEX"
    fi

    # Show exclude filter if set
    if [[ -n "$FILTER_REGEX" ]]; then
        echo -e "${BOLD}Exclude:${NC} $FILTER_REGEX"
    fi

    # Show subdirectories if set
    if [[ ${#SUBDIRS[@]} -gt 0 ]]; then
        echo -e "${BOLD}Subdirectories:${NC} ${SUBDIRS[*]}"
    fi

    echo ""

    # Find all album directories
    local album_dirs=()

    if [[ ${#SUBDIRS[@]} -gt 0 ]]; then
        # Search in specified subdirectories
        for subdir in "${SUBDIRS[@]}"; do
            if [[ -d "$subdir" ]]; then
                while IFS= read -r -d '' dir; do
                    album_dirs+=("$dir")
                done < <(find "$subdir" -maxdepth 1 -type d ! -name "$subdir" -print0 | sort -z)
            else
                echo -e "${YELLOW}Warning: Subdirectory '$subdir' not found, skipping${NC}"
            fi
        done
    else
        # Search in current directory
        while IFS= read -r -d '' dir; do
            album_dirs+=("$dir")
        done < <(find . -maxdepth 1 -type d ! -name "." -print0 | sort -z)
    fi

    local total_albums=${#album_dirs[@]}

    if [[ $total_albums -eq 0 ]]; then
        echo -e "${RED}No album directories found!${NC}"
        exit 1
    fi

    echo -e "${BOLD}Found ${total_albums} album directories${NC}"
    echo ""

    # Process each album
    local current=0
    for album_dir in "${album_dirs[@]}"; do
        current=$((current + 1))
        local album_name
        album_name="$(basename "$album_dir")"

        # Apply include filter if set (only process matching albums)
        if [[ -n "$INCLUDE_REGEX" ]]; then
            if ! [[ "$album_name" =~ $INCLUDE_REGEX ]]; then
                echo -e "${YELLOW}[${current}/${total_albums}]${NC} ${BOLD}Skipping:${NC} ${album_name} (not included)"
                skipped_albums=$((skipped_albums + 1))
                continue
            fi
        fi

        # Apply exclude filter if set (skip matching albums)
        if [[ -n "$FILTER_REGEX" ]]; then
            if [[ "$album_name" =~ $FILTER_REGEX ]]; then
                echo -e "${YELLOW}[${current}/${total_albums}]${NC} ${BOLD}Skipping:${NC} ${album_name} (excluded by filter)"
                skipped_albums=$((skipped_albums + 1))
                continue
            fi
        fi

        process_album "$album_dir" "$album_name" "$artist_name" "$current" "$total_albums"
        echo ""
    done

    # Print statistics
    echo -e "${BOLD}${MAGENTA}════════════════════════════════════════${NC}"
    echo -e "${BOLD}${MAGENTA}  Statistics${NC}"
    echo -e "${BOLD}${MAGENTA}════════════════════════════════════════${NC}"
    echo -e "${BOLD}Total albums found:${NC} ${total_albums}"
    if [[ $skipped_albums -gt 0 ]]; then
        echo -e "${BOLD}Skipped (filtered):${NC} ${YELLOW}${skipped_albums}${NC}"
    fi
    echo -e "${BOLD}Albums processed:${NC} ${GREEN}${processed_albums}${NC}"
    echo -e "${BOLD}Failed albums:${NC} ${RED}${failed_albums}${NC}"
    echo -e "${BOLD}Total tracks extracted:${NC} ${GREEN}${total_tracks}${NC}"
    echo ""

    if [[ $processed_albums -gt 0 ]]; then
        echo -e "${BOLD}Tracks per album:${NC}"

        # Find the longest album name (character count, not bytes)
        local max_width=0
        for album in "${!album_stats[@]}"; do
            local width=${#album}
            if [[ $width -gt $max_width ]]; then
                max_width=$width
            fi
        done

        # Print aligned table with optional release info
        for album in "${!album_stats[@]}"; do
            local release_info=""
            if [[ -n "${album_releases[$album]}" ]]; then
                release_info=" ${YELLOW}[${album_releases[$album]}]${NC}"
            fi
            printf "  ${CYAN}%-${max_width}s${NC} ${GREEN}%3d tracks${NC}%b\n" "$album" "${album_stats[$album]}" "$release_info"
        done | sort
    fi

    echo ""

    if [[ $failed_albums -gt 0 ]]; then
        exit 1
    fi
}

# Parse command-line arguments first (so --help works without tools)
parse_args "$@"

# Check for required tools
for tool in shnsplit iconv; do
    if ! command -v "$tool" &> /dev/null; then
        echo -e "${RED}Error: Required tool '$tool' not found${NC}" >&2
        exit 1
    fi
done

# Run main script
main
