{ flake, ... }:
final: prev: { caddy-cloudflare = flake.inputs.caddy-cloudflare.packages.${prev.system}.default; }
