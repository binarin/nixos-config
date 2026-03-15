FROM nixos/nix

# Configure nix to use the cache
RUN mkdir -p /etc/nix && echo 'substituters = http://niks3-storage.nix-cache:48080 https://cache.nixos.org/' > /etc/nix/nix.conf
RUN echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= binarin-niks3-cache-1:m3JYEypT3iWb02SzKOgIwdqEyDLXk1XO4UTVYFTQWxM=' >> /etc/nix/nix.conf

WORKDIR /workspace
COPY . /workspace/

# Pre-warm the nix store with all cached paths
# This fetches from the binary cache into the local store
RUN nix flake archive --json | jq -r '.. | .path? // empty' | xargs -I{} nix-store --realise {} || true
RUN nix build .#devShells.x86_64-linux.default --no-link
RUN nix build .#checks.x86_64-linux.check-flake-file --no-link
RUN nix build .#checks.x86_64-linux.treefmt --no-link
