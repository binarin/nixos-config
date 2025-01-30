# Like GNU `make`, but `just` rustier.
# https://just.systems/
# run `just` from this directory to see available commands

jobs := "auto"
topCacheDir := cache_directory() / "nixos-config"
nixOpts := "-v"

# Default command when 'just' is run without arguments
default:
    @just --list

# Update nix flake
[group('Main')]
update:
    nix run "$(pwd)#update"

# Lint nix files
[group('dev')]
lint:
    nix fmt

# Check nix flake
[group('dev')]
check:
    nix flake check {{ nixOpts }}

# Manually enter dev shell
[group('dev')]
dev:
    nix develop

# Activate the configuration
[group('Main')]
run:
    sudo time nixos-rebuild switch --flake "$(pwd)#$(hostname -s)" --keep-going -j {{ jobs }} {{ nixOpts }}

[group('Main')]
boot:
    sudo time nixos-rebuild boot --flake "$(pwd)#$(hostname -s)" --keep-going -j {{ jobs }} {{ nixOpts }}

[group('Main')]
build-hm host=`hostname -s` user="$USER":
    nix build "$(pwd)#nixosConfigurations.{{ host }}.config.home-manager.users.{{ user }}.home.activationPackage" --keep-going -j {{ jobs }} {{ nixOpts }} -o "{{ topCacheDir / 'home-configuration' / host / user }}"

[group('Main')]
hm host=`hostname -s` user="$USER":
    nix run --keep-going -j {{ jobs }} {{ nixOpts }} "$(pwd)#nixosConfigurations.{{ host }}.config.home-manager.users.{{ user }}.home.activationPackage"

[group('Main')]
build-nixos configuration=`hostname -s`:
    nix build "$(pwd)#nixosConfigurations.{{ configuration }}.config.system.build.toplevel" --keep-going -j {{ jobs }} {{ nixOpts }} -o "{{ topCacheDir / 'nixos-configuration' / configuration }}"


[group('Deploy')]
deploy target profile="system":
    deploy "$(pwd)#{{ target }}.{{ profile }}" -s -k -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}

[group('Deploy')]
deploy-boot target profile="system":
    deploy "$(pwd)#{{ target }}.{{ profile }}" --boot -s -k -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}
    ssh "root@{{ target }}" systemctl reboot

[group('Deploy')]
deploy-no-rollback target profile="system":
    deploy "$(pwd)#{{ target }}.{{ profile }}" --auto-rollback false --magic-rollback false -s -k -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}


[group('Deploy')]
lxc target:
    nix build "$(pwd)#nixosConfigurations.{{ target }}.config.formats.proxmox-lxc" --keep-going -j {{ jobs }} {{ nixOpts }} -o "proxmox-lxc-{{ target }}.tar.xz"

[group('Main')]
all: check
    #!/usr/bin/env bash
    set -euo pipefail
    mapfile -t all < <(nix flake show --json | jq  -r '.nixosConfigurations | keys | join("\n")')
    for cf in "${all[@]}"; do
      echo "Building $cf"
      just nixOpts="" build-nixos "$cf" || echo "Failed"
    done

[group('Main')]
deploy-all: all
    #!/usr/bin/env bash
    set -euo pipefail
    mapfile -t all < <(nix eval "$(pwd)#deploy.nodes" --apply builtins.attrNames --json | jq -r ".[]")
    for cf in "${all[@]}"; do
      echo "Deploying $cf"
      just nixOpts="" deploy "$cf" || echo "Failed"
    done

[group('Main')]
deploy-boot-all: all
    #!/usr/bin/env bash
    set -euo pipefail
    mapfile -t all < <(nix eval "$(pwd)#deploy.nodes" --apply builtins.attrNames --json | jq -r ".[]")
    for cf in "${all[@]}"; do
      echo "Deploying $cf"
      just nixOpts="" deploy-boot "$cf" || echo "Failed"
    done


[group('Ansible')]
ansible-inventory:
    nix build --impure --expr 'let pkgs = import <nixpkgs> {}; in (pkgs.formats.yaml {}).generate "public-keys.yaml" (import ./inventory/public-keys.nix)' -o ansible/ssh-public-keys.yaml

[group('Ansible')]
[working-directory: 'ansible']
ping-all:
    ansible --one-line all -m ping -u root

[group('CI')]
render-ci-workflows:
    nix run '.#ci-template-generator'

[group('dev')]
render-templates: ansible-inventory render-ci-workflows
