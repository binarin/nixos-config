#!/usr/bin/env zsh
set -euo pipefail
set -x

branches=(
    gh/epmd-systemd-mine
    gh/procps-systemd-mine
    gh/modern-rabbitmq-mine
)

cd /etc/nixos/nixpkgs-proposed
git fetch upstream
git fetch gh
git reset --hard upstream/master
git merge --no-edit --no-ff $branches
git push -f gh HEAD:refs/heads/propsed-prs
