# Like GNU `make`, but `just` rustier.
# https://just.systems/
# run `just` from this directory to see available commands

jobs := "auto"

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
    nix flake check

# Manually enter dev shell
[group('dev')]
dev:
    nix develop

# Activate the configuration
[group('Main')]
run:
    #nix run
    sudo time nixos-rebuild switch --flake "$(pwd)#$(hostname -s)" --keep-going -j {{ jobs }} -v -L

[group('Main')]
hm h=`hostname -s` u="$USER":
    nix build "$(pwd)#nixosConfigurations.{{ h }}.config.home-manager.users.{{ u }}.home.activationPackage" --keep-going -j {{ jobs }} -v -L

build-nixos configuration=`hostname -s`:
    nixos-rebuild build --flake "$(pwd)#{{ configuration }}" --keep-going -j {{ jobs }} -v -L
