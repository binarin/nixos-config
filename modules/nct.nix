{ self, ... }:
let
  selfLib = self.lib.self;

  # Common build inputs / env needed by the nix-bindings-* FFI crates
  # (pkg-config + bindgen against Nix's C headers). Mirrors upstream's
  # `nix-bindings-rust.nciBuildConfig`, simplified by building nct with
  # clangStdenv (so bindgen finds compiler headers without a gcc shim).
  nixBindingsBuild =
    { pkgs, lib }:
    let
      inherit (pkgs)
        pkg-config
        nix
        llvmPackages
        ;
      libclang = llvmPackages.clang-unwrapped.lib;
    in
    {
      deps = {
        nativeBuildInputs = [
          pkg-config
        ];
        buildInputs = [
          nix
          # stdbool.h and friends
          pkgs.stdenv.cc
        ];
        env = {
          LIBCLANG_PATH = "${libclang}/lib";
          BINDGEN_EXTRA_CLANG_ARGS = "-x c++ -std=c++2a";
        };
      };
      shell = {
        packages = [
          nix
          pkg-config
          llvmPackages.clang-unwrapped
        ];
        env = {
          LIBCLANG_PATH = "${libclang}/lib";
          BINDGEN_EXTRA_CLANG_ARGS = "-x c++ -std=c++2a";
        };
      };
    };

  packageFn =
    { rustPlatform, lib, pkgs }:
    let
      build = (nixBindingsBuild { inherit pkgs lib; }).deps;
    in
    rustPlatform.buildRustPackage (
      {
        pname = "nct";
        version = "0.1.0";
        src = selfLib.dir' "tools/nct";
        cargoLock = {
          lockFile = selfLib.file' "tools/nct/Cargo.lock";
          # nix-bindings-* crates are git deps; fetch them via builtins so we
          # don't have to maintain outputHashes that drift on every upstream
          # bump. These are public, content-addressed git revs.
          allowBuiltinFetchGit = true;
        };
        meta = {
          mainProgram = "nct";
          description = "nixos-config-tool: CLI for NixOS configuration management";
        };
      }
      // build
    );
in
{
  perSystem =
    { pkgs, lib, ... }:
    let
      # Use clang stdenv so bindgen finds compiler headers (features.h, etc.)
      # without a gcc include-path shim.
      rustPlatform = pkgs.makeRustPlatform {
        inherit (pkgs) cargo rustc;
        stdenv = pkgs.clangStdenv;
      };
      # bindgen (via libclang) doesn't read the stdenv cc wrapper, so add the
      # libc + cc include paths explicitly. clangStdenv gives us these via
      # its cc and libc_dev.
      clangIncludeArgs = lib.concatStringsSep " " (
        ["-x c++ -std=c++2a"]
        ++ map (p: "-I${p}") ["${pkgs.glibc.dev}/include"]
      );
      shellBuild = (nixBindingsBuild { inherit pkgs lib; }).shell;
    in
    {
      packages.nct = pkgs.callPackage packageFn { inherit rustPlatform; };

      devShells.nct = pkgs.clangStdenv.mkDerivation {
        name = "nct";
        nativeBuildInputs = with pkgs; [
          cargo
          rustc
          rust-analyzer
          clippy
          rustfmt
          pkg-config
          llvmPackages.clang-unwrapped
        ];
        buildInputs = [ pkgs.nix ];
        LIBCLANG_PATH = "${pkgs.llvmPackages.clang-unwrapped.lib}/lib";
        BINDGEN_EXTRA_CLANG_ARGS = clangIncludeArgs;
      };
    };

  flake.overlays.nct = final: _prev: {
    nct =
      let
        rustPlatform = final.makeRustPlatform {
          inherit (final) cargo rustc;
          stdenv = final.clangStdenv;
        };
      in
      final.callPackage packageFn { inherit rustPlatform; };
  };
}
