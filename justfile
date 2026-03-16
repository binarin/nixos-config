# Like GNU `make`, but `just` rustier.
# https://just.systems/
# run `just` from this directory to see available commands

jobs := "auto"
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
    ./scripts/check-module-keys.sh
    nix fmt

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
hm host=`hostname -s` user="$USER":
    nix run --keep-going -j {{ jobs }} {{ nixOpts }} "$(pwd)#nixosConfigurations.{{ host }}.config.home-manager.users.{{ user }}.home.activationPackage"

# Evaluate a single configuration without building (useful for debugging infinite recursion)
[group('Main')]
eval-nixos configuration=`hostname -s`:
    ncf eval nixos "{{ configuration }}"

# Evaluate all configurations individually (useful for debugging infinite recursion and isolating failures)
[group('Main')]
eval-all:
    ncf eval all

[group('Main')]
build-all: check
    ncf build all


[group('Ansible')]
ansible-inventory:
    ncf generate ansible-inventory

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
