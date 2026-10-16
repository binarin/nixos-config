# Vendored from upstream's own nix expression (headless daemon + CLI):
#   https://github.com/getpaseo/paseo/blob/v0.11.4/nix/package.nix
#
# Adaptations for this repo:
#   - src: fetchFromGitHub of the release tag instead of `cleanSourceWith ./..`
#     (the source filter is unnecessary for a fetched tarball).
#   - version + npmDepsHash pinned inline (upstream reads ../package.json at
#     eval time and keeps the deps hash in a sidecar file).
#
# Updating: bump `version`, refresh the `src` hash, then rebuild with a wrong
# `npmDepsHash` on purpose and replace it with the "got:" hash nix reports
# (the npm-deps fetcher in our nixpkgs pin differs from upstream's).
let
  packageFn =
    {
      lib,
      stdenv,
      buildNpmPackage,
      fetchFromGitHub,
      nodejs_22,
      python3,
      makeWrapper,
      autoPatchelfHook,
      # node-pty needs libuv headers on Linux
      libuv,
    }:
    buildNpmPackage rec {
      pname = "paseo";
      version = "0.11.4";

      src = fetchFromGitHub {
        owner = "getpaseo";
        repo = "paseo";
        tag = "v${version}";
        hash = "sha256-UhQVi9qq2WaUKGwyxfis4JN8qhMJH5i8EvvA58wO0IM=";
      };

      nodejs = nodejs_22;

      npmDepsHash = "sha256-nBFW6cm/4LO3q9BAsZSjBoNQN1i1PshCfcKbup0G/Lg=";

      # Prevent onnxruntime-node's install script from running during automatic
      # npm rebuild (it tries to download from api.nuget.org, which fails in the sandbox).
      # We manually rebuild only node-pty in buildPhase.
      npmRebuildFlags = [ "--ignore-scripts" ];

      nativeBuildInputs = [
        python3 # for node-gyp (node-pty compilation)
        makeWrapper
      ] ++ lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];

      buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
        libuv
        stdenv.cc.cc.lib # libstdc++ for sherpa-onnx prebuilt binaries
      ];

      # Don't use the default npm build hook — we need a custom build sequence
      dontNpmBuild = true;

      buildPhase = ''
        runHook preBuild

        # Rebuild only node-pty (native addon for terminal emulation). The sherpa
        # speech runtime ships prebuilt platform packages and is copied into the
        # daemon closure by scripts/trace-daemon.mjs.
        npm rebuild node-pty

        # Build all server packages in dependency order (defined in package.json)
        npm run build:server
        npm run build:daemon-web-ui

        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall

        # Compute the daemon's runtime closure by static module-graph tracing
        # (@vercel/nft from supervisor-entrypoint.js, cli/dist/index.js, and the
        # forked terminal/speech worker processes) plus an explicit list of non-JS
        # assets read at runtime. The trace script is the single source of
        # truth for what the daemon needs at $out — auditable in plain JS, no
        # npm hoisting / .bin / workspace-symlink footguns.
        mkdir -p $out/lib/paseo
        node scripts/trace-daemon.mjs > daemon-files.txt

        while IFS= read -r path; do
          [ -z "$path" ] && continue
          mkdir -p "$out/lib/paseo/$(dirname "$path")"
          cp -a "$path" "$out/lib/paseo/$path"
        done < daemon-files.txt

        # Shell hooks invoke the retained CLI bin directly, without a system Node.
        patchShebangs --build "$out/lib/paseo"

        # Root package.json lets node resolve the workspace layout when the
        # CLI/server bin starts from $out.
        cp package.json $out/lib/paseo/

        # Web UI Assets
        cp -r packages/server/dist/server/web-ui $out/lib/paseo/packages/server/dist/server/

        # Create wrapper for the server entry point (for systemd / direct use)
        mkdir -p $out/bin
        # Keep Paseo's runtime mode separate from NODE_ENV, which belongs to spawned agents.
        makeWrapper ${nodejs}/bin/node $out/bin/paseo-server \
          --add-flags "$out/lib/paseo/packages/server/dist/scripts/supervisor-entrypoint.js" \
          --set PASEO_NODE_ENV production

        # Create wrapper for the CLI
        makeWrapper ${nodejs}/bin/node $out/bin/paseo \
          --add-flags "$out/lib/paseo/packages/cli/dist/index.js" \
          --set NODE_PATH "$out/lib/paseo/node_modules"

        runHook postInstall
      '';

      meta = {
        description = "Self-hosted daemon for Claude Code, Codex, and OpenCode";
        homepage = "https://github.com/getpaseo/paseo";
        changelog = "https://github.com/getpaseo/paseo/releases/tag/v${version}";
        license = lib.licenses.agpl3Plus;
        mainProgram = "paseo";
        platforms = lib.platforms.linux ++ lib.platforms.darwin;
      };
    };
in
{
  perSystem =
    { pkgs, ... }:
    {
      packages.paseo = pkgs.callPackage packageFn { };
    };

  flake.overlays.paseo = final: _prev: {
    paseo = final.callPackage packageFn { };
  };
}
