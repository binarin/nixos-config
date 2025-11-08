# Like GNU `make`, but `just` rustier.
# https://just.systems/
# run `just` from this directory to see available commands

jobs := "auto"
topCacheDir := cache_directory() / "nixos-config"
nixOpts := "-v"
sshOpts := ""

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
    ./scripts/build-nixos.sh "{{ configuration }}" "{{ topCacheDir / 'nixos-configuration' / configuration }}" -j {{ jobs }} {{ nixOpts }}

# Evaluate a single configuration without building (useful for debugging infinite recursion)
[group('Main')]
eval-nixos configuration=`hostname -s`:
    ./scripts/eval-nixos.sh "{{ configuration }}" {{ nixOpts }}

# Evaluate all configurations individually (useful for debugging infinite recursion and isolating failures)
[group('Main')]
eval-all:
    ./scripts/eval-all-nixos.sh {{ nixOpts }}

[group('Deploy')]
iso:
    nix build "$(pwd)#nixosConfigurations.iso.config.system.build.isoImage" --keep-going -j {{ jobs }} {{ nixOpts }} -o "{{ topCacheDir / 'nixos-configuration' / 'iso' }}"


[group('Deploy')]
deploy target profile="system":
    deploy "$(pwd)#{{ target }}.{{ profile }}" -s -k -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}

[group('Deploy')]
deploy-boot target profile="system":
    deploy "$(pwd)#{{ target }}.{{ profile }}" --boot -s -k --ssh-opts="{{ sshOpts }}" -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}
    ssh "root@$(nix eval "$(pwd)#nixosConfigurations.{{ target }}.config.hostConfig.deployHostName" --json | jq -r)" systemctl reload dbus-broker.service
    ssh "root@$(nix eval "$(pwd)#nixosConfigurations.{{ target }}.config.hostConfig.deployHostName" --json | jq -r)" systemctl reboot

[group('Deploy')]
deploy-no-rollback target profile="system":
    deploy "$(pwd)#{{ target }}.{{ profile }}" --auto-rollback false --magic-rollback false -s -k --ssh-opts="{{ sshOpts }}" -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}


[group('Deploy')]
lxc target:
    nix build "$(pwd)#nixosConfigurations.{{ target }}.config.system.build.tarball" --keep-going -j {{ jobs }} {{ nixOpts }} -o "proxmox-lxc-{{ target }}"

[group('Deploy')]
lxc-upload target host="raum": (lxc target)
    rsync -vL proxmox-lxc-{{ target }}/tarball/nixos-system-x86_64-linux.tar.xz root@{{ host }}:/var/lib/vz/template/cache/proxmox-lxc-{{ target }}.tar.xz

[group('Deploy')]
lxc-create target id host="raum": (lxc-upload target)
    nix eval "$(pwd)#nixosConfigurations.docker-on-nixos.config.lib.lxc.createCommand" --apply 'f: f "{{ id }}"' --json | jq -r . | ssh root@raum bash -xeu -

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
    #!/usr/bin/env bash
    set -euo pipefail
    # Build ssh-public-keys.yaml
    outpath=$(nix build --impure --print-out-paths --no-link --expr 'let pkgs = import <nixpkgs> {}; in (pkgs.formats.yaml {}).generate "public-keys.yaml" (import ./inventory/public-keys.nix)')
    cp -f "$outpath" ansible/ssh-public-keys.yaml
    # Build ip-allocation.yaml
    outpath=$(nix build --impure --print-out-paths --no-link --expr 'let pkgs = import <nixpkgs> {}; fl = builtins.getFlake "'$(pwd)'"; in (pkgs.formats.yaml {}).generate "public-keys.yaml" { ip_allocation = fl.helpers.networks-lookup.buildHostLookupTable (fl.helpers.networks-lookup.readRawInventory);}')
    cp -f "$outpath" ansible/ip-allocation.yaml

[group('Ansible')]
[working-directory: 'ansible']
ping-all:
    ansible --one-line all -m ping -u root

[group('Ansible')]
[working-directory: 'ansible']
ansible-deps: # ansible-inventory
    ansible-galaxy collection install -r requirements.yml
    ansible-galaxy role install -r requirements.yml

[group('CI')]
render-ci-workflows:
    nix run '.#ci-template-generator'

[group('dev')]
render-templates: ansible-inventory render-ci-workflows
