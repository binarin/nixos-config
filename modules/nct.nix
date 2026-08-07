{ self, inputs, ... }:
let
  selfLib = self.lib.self;
  # `inputs` is used via the `imports` below (inputs.nci.flakeModule,
  # inputs.nix-bindings-rust.modules.flake.basic).
in
{
  flake-file.inputs = {
    nci = {
      url = "github:90-008/nix-cargo-integration";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # For the nciBuildConfig + input-propagation-workaround flake-parts module.
    # (The Cargo crates themselves still come via the git dep in Cargo.toml.)
    nix-bindings-rust = {
      url = "github:nixops4/nix-bindings-rust";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  imports = [
    inputs.nci.flakeModule
    # Makes perSystem.nix-bindings-rust.{nciBuildConfig,nixPackage} available.
    inputs.nix-bindings-rust.modules.flake.basic
  ];

  perSystem =
    { pkgs, config, lib, ... }:
    let
      nciOut = config.nci.outputs."nct";
      # nci per-profile packages: dev, release.
      nctRelease = nciOut.packages.release;
    in
    {
      nci.projects."nct".path = selfLib.dir' "tools/nct";

      nci.crates."nct" = {
        export = true;
        # Use clangStdenv for both the deps derivation and the main one so
        # bindgen finds compiler headers (features.h, stdbool.h, ...) without
        # upstream's gcc bindgen shim.
        #
        # nciBuildConfig (pkg-config, libclang, bindgen args + the
        # input-propagation-workaround that auto-adds the nix-*-c libraries)
        # must go into depsDrvConfig: the nix-bindings-*-sys crates run
        # build.rs (pkg-config + bindgen) during the *deps* build.
        # (See nix-bindings-rust/nci.nix: "Downstream projects import this
        # into depsDrvConfig instead".)
        depsDrvConfig = {
          imports = [ config.nix-bindings-rust.nciBuildConfig ];
          deps.stdenv = pkgs.clangStdenv;
        };
        drvConfig = {
          imports = [ config.nix-bindings-rust.nciBuildConfig ];
          deps.stdenv = pkgs.clangStdenv;
        };
      };

      # Wrap the nci-built binary so guestfish (libguestfs-with-appliance)
      # is on PATH at runtime — nct shells out to it for `provision-pve
      # --inject-key` to bake the clan age key into the qcow2. The
      # `-with-appliance` variant bundles the supermin appliance guestfish
      # needs to boot its helper VM; plain libguestfs is unusable alone.
      packages.nct = pkgs.runCommand "nct-${nctRelease.version}"
        {
          nativeBuildInputs = [ pkgs.makeWrapper ];
          meta = nctRelease.meta // { mainProgram = "nct"; };
        }
        ''
          makeWrapper ${lib.getExe' nctRelease "nct"} $out/bin/nct \
            --prefix PATH : ${lib.makeBinPath [ pkgs.libguestfs-with-appliance ]}
        '';

      # Expose nci's native clippy / check outputs as flake checks, built with
      # the same toolchain nci uses for the package (avoids the rustc/clippy
      # version skew you'd hit by adding pkgs.clippy to the dev shell).
      checks.nct-clippy = nciOut.clippy;
      checks.nct-test = nciOut.check;

      devShells.nct = nciOut.devShell.overrideAttrs (old: {
        # nci's dev shell already adds the full Rust toolchain from its
        # `mkShell` toolchain (cargo, rustc, cargo-clippy, clippy-driver,
        # rustfmt, cargo-fmt — all at a consistent version). Only rust-analyzer
        # isn't bundled in the rust toolchain, so add it from nixpkgs. Do NOT
        # add pkgs.clippy / pkgs.rustfmt: they'd come from a different rustc
        # than the toolchain and cause E0514 "incompatible version" errors.
        packages = (old.packages or [ ]) ++ [
          pkgs.rust-analyzer
          # Runtime dep nct shells out to for image key injection
          # (provision-pve --inject-key). Plain libguestfs is unusable: it
          # needs the supermin appliance only -with-appliance ships.
          pkgs.libguestfs-with-appliance
        ];
        # nciBuildConfig sets BINDGEN_EXTRA_CLANG_ARGS="-x c++ -std=c++2a",
        # but in the dev shell bindgen calls libclang directly (bypassing the
        # cc wrapper), so it can't find glibc's features.h. Add the include
        # path explicitly. (Same fix as Stage 2.)
        BINDGEN_EXTRA_CLANG_ARGS = "${old.BINDGEN_EXTRA_CLANG_ARGS or "-x c++ -std=c++2a"} -I${pkgs.glibc.dev}/include";
      });
    };
}
# NOTE: the Stage 2 `flake.overlays.nct` was dropped — it had no consumers
# (not in modules/nix.nix defaultOverlays, nothing uses pkgs.nct). nct is only
# consumed as a flake output (self'.packages.nct in devshell.nix). Re-add a
# plain overlay if a consumer appears.
