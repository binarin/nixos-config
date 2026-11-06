# nixos-config-tool (`nct`)

Rust-based CLI for NixOS configuration management. Companion / eventual
successor-ish to the Python-based `ncf` tool.

## Commands

```
nct machine provision <name>            # stub (prints the machine name)
nct machine eval-expr <name> <expr>     # eval a Rust-like lambda against a nixosConfiguration
nct machine eval-expr <name> <expr> -f <flake-ref>
```

`machine eval-expr` mirrors `nix eval --apply`:

1. Loads this flake's `nixosConfigurations.<name>` (a full NixOS module eval)
   as a Nix value, via `builtins.getFlake`.
2. Applies the user lambda (`m: ...`) to that value (`m` is the whole
   top-level machine attrset, so `m.config.X` is the access pattern).
3. Prints the result: strings verbatim, lists/attrsets Nix-style.

### Example

```sh
nct machine eval-expr llm-runner 'm: builtins.attrNames m.config.llama-models.configurations'
# => [ "gemma4"; "granite-4.1-30b"; ... ]
```

The nix evaluation happens in-process via [`nix-bindings-rust`] (FFI to Nix's
C API), not by shelling out to `nix eval`.

[`nix-bindings-rust`]: https://github.com/nixops4/nix-bindings-rust

## Development

```sh
nix develop ../../#nct         # rust toolchain (cargo, clippy, rustfmt, rust-analyzer)
cargo test                     # unit tests
cargo test -- --ignored        # integration tests (need a working nix store)
cargo clippy --all-targets -- -D warnings
```

The dev shell is built by [nix-cargo-integration] (nci). It adds the full Rust
toolchain at a single consistent version — do **not** `nixpkgs`-override
`clippy`/`rustfmt`, or you'll hit `E0514 "incompatible version"` (rustc skew).
`LIBCLANG_PATH` / `BINDGEN_EXTRA_CLANG_ARGS` are set so the
`nix-bindings-*-sys` crates (pkg-config + bindgen against Nix's C headers)
build out of the box.

[nix-cargo-integration]: https://github.com/90-008/nix-cargo-integration

## Build

```sh
nix build ../../#nct           # flake-level package (release profile)

# native nci outputs (built with the same toolchain as the package):
nix build ../../#checks.x86_64-linux.nct-clippy
nix build ../../#checks.x86_64-linux.nct-test
```

## How it's wired (flake side)

`modules/nct.nix` is a flake-parts module that:

- declares `nci` + `nix-bindings-rust` as flake inputs (via `flake-file`),
- imports `inputs.nci.flakeModule` and
  `inputs.nix-bindings-rust.modules.flake.basic` (which provides
  `perSystem.nix-bindings-rust.nciBuildConfig`),
- configures `nci.projects."nct"` / `nci.crates."nct"`, importing
  `nciBuildConfig` into **both** `drvConfig` and `depsDrvConfig` (the
  `*-sys` crates run `build.rs` during the deps build) and using `clangStdenv`
  so bindgen finds compiler headers,
- exposes `packages.nct`, the `nct-clippy` / `nct-test` flake checks, and the
  `devShells.nct`.

See `todo/nct.org` (Stages 0–3) for the full design history.
