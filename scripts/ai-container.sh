#!/usr/bin/env bash
set -euo pipefail

if [ "$EUID" -ne 0 ]
  then echo "Root privileges needed to start container"
  exit
fi

container=$1

profile=/nix/var/nix/profiles/per-container/$container
gcroots=/nix/var/nix/gcroots/per-container/$container
root=/var/lib/machines/$container

config=$(nix build .#nixosConfigurations.$container.config.system.build.toplevel --print-out-paths --no-link)

mkdir -p $profile /nix/var/nix/gcroots/per-container/$container
nix-env --set $config --profile $profile/system

mkdir -p $root

systemd-nspawn \
  --machine $container \
  --directory $root \
  --bind-ro=/nix/store \
  --bind-ro=/nix/var/nix/db \
  --bind-ro=/nix/var/nix/daemon-socket \
  --bind="$profile:/nix/var/nix/profiles" \
  --bind="$gcroots:/nix/var/nix/gcroots" \
  --bind="/home/binarin/personal-workspace/nixos-config" \
  --bind="/home/binarin/.claude" \
  --bind="/home/binarin/.claude.json" \
  $config/init
