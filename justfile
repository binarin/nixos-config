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
    ./scripts/check-module-keys.sh
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

# Check if git-crypt is unlocked (fails if repository is locked)
[private]
check-git-crypt-unlocked:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ -z "$(git config --local --get filter.git-crypt.smudge)" ]; then
        echo "ERROR: Repository is locked with git-crypt." >&2
        echo "Please unlock it first with: git-crypt unlock" >&2
        exit 1
    fi

[group('Deploy')]
deploy target profile="system": check-git-crypt-unlocked
    deploy "$(pwd)#{{ target }}.{{ profile }}" -s -k -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}

[group('Deploy')]
deploy-boot target profile="system": check-git-crypt-unlocked
    deploy "$(pwd)#{{ target }}.{{ profile }}" --boot -s -k --ssh-opts="{{ sshOpts }}" -r "{{ topCacheDir / 'deploy-rs' }}" -- {{ nixOpts }}
    ssh "root@$(nix eval "$(pwd)#deploy.nodes.{{ target }}.hostname" --json | jq -r)" systemctl reload dbus-broker.service
    ssh "root@$(nix eval "$(pwd)#deploy.nodes.{{ target }}.hostname" --json | jq -r)" systemctl reboot

[group('Deploy')]
deploy-no-rollback target profile="system": check-git-crypt-unlocked
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
deploy-all: check-git-crypt-unlocked all
    #!/usr/bin/env bash
    set -euo pipefail
    mapfile -t all < <(nix eval "$(pwd)#deploy.nodes" --apply builtins.attrNames --json | jq -r ".[]")
    for cf in "${all[@]}"; do
      echo "Deploying $cf"
      just nixOpts="" deploy "$cf" || echo "Failed"
    done

[group('Main')]
deploy-boot-all: check-git-crypt-unlocked all
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
    outpath=$(nix build --impure --print-out-paths --no-link --expr 'let pkgs = import <nixpkgs> {}; fl = builtins.getFlake "'$(pwd)'"; in (pkgs.formats.yaml {}).generate "public-keys.yaml" (let networks-lookup = import ./lib/networks-lookup.nix { self = fl; lib = pkgs.lib; }; in { ip_allocation = networks-lookup.buildHostLookupTable (networks-lookup.readRawInventory);})')
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
    ncf ci generate

[group('dev')]
render-templates: ansible-inventory render-ci-workflows

[group('dev')]
wb:
    nix build '.#nixosConfigurations.demandred.config.home-manager.users.binarin.xdg.configFile."waybar/config".source' -o ~/.config/waybar/config
    nix build '.#nixosConfigurations.demandred.config.home-manager.users.binarin.programs.waybar.style' -o ~/.config/waybar/style.css

[group('dev')]
em:
    #!/usr/bin/env bash
    set -euo pipefail
    cfg=$(nix build '.#nixosConfigurations.demandred.config.home-manager.users.binarin.programs.custom-emacs.compiledConfig' --no-link --print-out-paths)
    for file in {early-,}init.el{,c} ; do
      ln -sf $cfg/$file ~/.config/emacs/$file
    done

# Check if arion docker images have newer versions available
[group('dev')]
check-arion-images:
    nix shell nixpkgs#jq nixpkgs#curl -c ./scripts/check-arion-images.sh
