#!/usr/bin/env bash
set -euo pipefail
set -x
if [[ -z $1 ]]; then
    echo "Usage: $0 hostname"
    exit 1
fi

host=$1

luks_pw_file=/tmp/deploy-luks-password-without-newline

# if [[ ! -f $luks_pw_file ]]; then
#     read -s -p "LUKS password: " luks_pw
#     echo -n "$luks_pw" > "$luks_pw_file"
# fi

# Create a temporary directory
temp=$(mktemp -d)

# Function to cleanup temporary directory on exit
cleanup() {
  rm -rf "$temp"
}
#trap cleanup EXIT

while read -r line; do
    file=$(basename $line)
    source=secrets/$host/$file

    target_dir=$temp/$(dirname $line)
    target=$temp/$line
    install -d -m755 "$target_dir"
    sops decrypt "$source" > "$target"
    chmod 0600 "$target"
done < <(nix eval "$(pwd)#nixosConfigurations.$host.config.services.openssh.hostKeys" --json | jq '.[]|.path' -r)

binarin_age=$(nix eval "$(pwd)#nixosConfigurations.furfur.config.home-manager.users.binarin.sops.age.keyFile" --json | jq . -r)
binarin_age_dir=$(dirname $binarin_age)

install -d -m700 "$temp/persist/home/binarin"
install -d -m700 "$temp/local/home/binarin"

install -d -m700 "$temp/$binarin_age_dir"
sops decrypt "secrets/$host/user-binarin-age" > "$temp/$binarin_age"
chmod 0600 "$temp/$binarin_age"

binarin_uid=$(nix eval "$(pwd)#nixosConfigurations.furfur.config.users.users.binarin.uid")
binarin_gid=$(nix eval "$(pwd)#nixosConfigurations.furfur.config.users.groups.binarin.gid")

nix \
    run github:nix-community/nixos-anywhere -- \
    --generate-hardware-config nixos-generate-config "machines/$host/hardware-configuration.nix" \
    --flake "$(pwd)#$host" \
    --target-host root@iso \
    --extra-files "$temp" \
    --chown "/persist/home/binarin" "$binarin_uid:$binarin_gid" \
    --chown "/local/home/binarin" "$binarin_uid:$binarin_gid"

#    --builders 'ssh://nix-cache x86_64-linux /home/binarin/.ssh/id_ed25519 16 1 big-parallel' --max-jobs 0 \
#    --disk-encryption-keys "$luks_pw_file" "$luks_pw_file" \
