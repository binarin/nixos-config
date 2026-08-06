# nixos-config-tool (`nct`)

Rust-based CLI for NixOS configuration management. Currently a scaffold with a
`nct machine provision` stub.

## Development

```sh
nix develop ../../#nct         # rust toolchain (cargo, rust-analyzer, clippy, rustfmt)
cargo test
```

## Build

```sh
nix build ../../#nct           # flake-level package
```
