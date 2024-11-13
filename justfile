# Like GNU `make`, but `just` rustier.
# https://just.systems/
# run `just` from this directory to see available commands

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
  sudo time nixos-rebuild switch --flake $(pwd)#$(hostname -s) --keep-going --show-trace -j8 -v

[group('Main')]
hm:
  nix run "$(pwd)#nixosConfigurations.valak.config.home-manager.users.binarin.home.activationPackage" -v --keep-going --show-trace -j8
