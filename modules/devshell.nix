{
  perSystem =
    {
      self',
      inputs',
      pkgs,
      ...
    }:
    let
      # Environment variables to preserve when using --ignore-env with nix
      # Essential for nix operations in CI environments
      envVarsToKeep = [
        # Essential for nix operations
        "HOME"
        "USER"
        "LANG"
        "LOCALE_ARCHIVE"
        "TZDIR"
        "TERM"
        "TMPDIR"
        "TMP"
        "TEMP"
        "PATH"
        "SSL_CERT_FILE"
        "NIX_SSL_CERT_FILE"
        # XDG Base Directory Specification
        "XDG_CACHE_HOME"
        "XDG_CONFIG_HOME"
        "XDG_DATA_HOME"
        "XDG_STATE_HOME"
        "XDG_RUNTIME_DIR"
        # XDG User Directories
        "XDG_DESKTOP_DIR"
        "XDG_DOCUMENTS_DIR"
        "XDG_DOWNLOAD_DIR"
        "XDG_MUSIC_DIR"
        "XDG_PICTURES_DIR"
        "XDG_PUBLICSHARE_DIR"
        "XDG_TEMPLATES_DIR"
        "XDG_VIDEOS_DIR"
      ];

      envKeepFlags = pkgs.lib.concatMapStringsSep " " (v: "-k ${v}") envVarsToKeep;

      env-aware-nix-run = pkgs.writeShellScriptBin "env-aware-nix-run" ''
        # Wrapper for 'nix run --ignore-env' that preserves essential environment variables
        # Used in CI to get reproducible builds while keeping vars nix needs (HOME, XDG_*, etc.)
        exec ${pkgs.nix}/bin/nix run --ignore-env ${envKeepFlags} "$@"
      '';

      env-aware-nix-develop = pkgs.writeShellScriptBin "env-aware-nix-develop" ''
        # Wrapper for 'nix develop --ignore-environment' that preserves essential environment variables
        # Used in CI to get reproducible builds while keeping vars nix needs (HOME, XDG_*, etc.)
        exec ${pkgs.nix}/bin/nix develop --ignore-environment ${envKeepFlags} "$@"
      '';

      git-crypt-patched = pkgs.git-crypt.overrideAttrs (_prev: {
        patches = [
          (pkgs.fetchpatch {
            url = "https://github.com/AGWA/git-crypt/commit/2da5e0016e53aba381046063c24c07f1bee3d824.diff";
            sha256 = "sha256-fyHS2oeElUh+KEtvfnpf2/IiJPNSu03af+ilYFm3wOU";
          })
        ];
      });

      ncfRuntimeDeps = with pkgs; [
        nix
        nix-output-monitor # nom for nicer build output
        git
        git-crypt-patched
        openssh # ssh-keygen
        age # age-keygen
        sops
        ssh-to-age
        yamlfmt
        apg
        deploy-rs # For ncf deploy commands
        libarchive # bsdtar for efficient tarball manipulation
        zstd # Fast compression for LXC tarballs
        pv # Progress monitoring for data pipelines
        fzf # Interactive selection for ncf ts auth-key
      ];

      ncf = pkgs.python3.pkgs.buildPythonApplication {
        pname = "ncf";
        version = "0.1.0";
        format = "pyproject";

        src = ../tools/ncf;

        nativeBuildInputs = [ pkgs.makeWrapper ];

        build-system = [ pkgs.python3.pkgs.setuptools ];

        dependencies = with pkgs.python3.pkgs; [
          pyyaml
          typer
          rich
          ruamel-yaml
          pydantic
          gitpython
          jinja2
          tomlkit
          proxmoxer
          paramiko
          inputs'.clan-core.packages.clan-cli
        ];

        postFixup = ''
          wrapProgram $out/bin/ncf \
            --prefix PATH : ${pkgs.lib.makeBinPath ncfRuntimeDeps}
        '';
      };

      ncfPythonEnv = pkgs.python3.withPackages (ps: ncf.dependencies ++ [ ps.pytest ]);

      # Grafana Foundation SDK for dashboard generation
      grafana-foundation-sdk = pkgs.python3.pkgs.buildPythonPackage {
        pname = "grafana-foundation-sdk";
        version = "11.6.0";
        format = "wheel";

        src = pkgs.fetchurl {
          url = "https://files.pythonhosted.org/packages/ef/82/16606327d24855f90a2fcbff735c8dd2cc9e3d7db71676cb65c3293a7083/grafana_foundation_sdk-1769699379%2111.6.0-py3-none-any.whl";
          hash = "sha256-kteoFE66D6gC5yj7cOY9dLBW+xGV/xFtgSrXpMz6Xso=";
          name = "grafana_foundation_sdk-11.6.0-py3-none-any.whl";
        };

        pythonImportsCheck = [ "grafana_foundation_sdk" ];
      };

      # Monitoring tool for generating Grafana dashboards
      monitoring = pkgs.python3.pkgs.buildPythonApplication {
        pname = "monitoring";
        version = "0.1.0";
        format = "pyproject";

        src = ../monitoring;

        build-system = [ pkgs.python3.pkgs.setuptools ];

        dependencies = [
          grafana-foundation-sdk
          pkgs.python3.pkgs.pyyaml
        ];

        pythonImportsCheck = [ "monitoring" ];
      };

      monitoringPythonEnv = pkgs.python3.withPackages (ps: [
        grafana-foundation-sdk
        ps.pyyaml
      ]);

    in
    {
      packages.ncf = ncf;
      packages.monitoring = monitoring;
      packages.env-aware-nix-run = env-aware-nix-run;
      packages.env-aware-nix-develop = env-aware-nix-develop;
      packages.git-crypt-patched = git-crypt-patched;

      # Check that a built system closure doesn't reference the flake source tree.
      #
      # Strategy: scan all -source paths in the closure and check if any contain
      # our repo's marker files (flake.nix + modules/ + files/). This is more
      # reliable than the previous manifest.json approach, which missed leaks
      # because nixos-system closures don't contain a manifest.json.
      #
      # Usage: check-no-flake-source-leak /nix/store/...-nixos-system-...
      packages.check-no-flake-source-leak = pkgs.writeShellApplication {
        name = "check-no-flake-source-leak";
        # Use nix from PATH (system nix) rather than bundling ${pkgs.nix},
        # because a bundled nix may have a different store database and fail
        # to query paths that the system nix can see.
        runtimeInputs = [ ];
        text = ''
          if [ $# -eq 0 ]; then
            echo "Usage: check-no-flake-source-leak <store-path> [<store-path>...]"
            exit 1
          fi

          FAILED=0

          is_our_flake_source() {
            local p="$1"
            # Our repo has flake.nix, modules/, and files/ at the top level
            [[ -f "$p/flake.nix" ]] && [[ -d "$p/modules" ]] && [[ -d "$p/files" ]]
          }

          for STORE_PATH in "$@"; do
            # Find all -source paths in the closure
            SOURCES=$(nix-store -qR "$STORE_PATH" 2>/dev/null | grep -- '-source$' || true)
            if [ -z "$SOURCES" ]; then
              echo "OK   $STORE_PATH (no -source paths in closure)"
              continue
            fi

            PATH_FAILED=0
            while IFS= read -r SRC; do
              if is_our_flake_source "$SRC"; then
                echo "FAIL $STORE_PATH — flake source leaked: $SRC"
                # Show what references it
                REFERRERS=$(nix-store -q --referrers "$SRC" 2>/dev/null \
                  | grep -v '\.drv$' \
                  | while read -r ref; do
                      nix-store -qR "$STORE_PATH" 2>/dev/null | grep -qF "$ref" 2>/dev/null && echo "  $ref"
                    done || true)
                if [ -n "$REFERRERS" ]; then
                  echo "  Referenced by:"
                  echo "$REFERRERS"
                fi
                PATH_FAILED=1
              fi
            done <<< "$SOURCES"

            if [ "$PATH_FAILED" -eq 0 ]; then
              echo "OK   $STORE_PATH"
            else
              FAILED=1
            fi
          done

          if [ "$FAILED" -ne 0 ]; then
            echo ""
            echo "FAILED: Flake source tree found in closure."
            echo "Use selfLib.{file,dir,read} (from self.lib.self) instead of \''${self}."
            exit 1
          fi

          echo ""
          echo "All store paths are clean."
        '';
      };

      devShells.niri-dynamic-keybindings = pkgs.mkShell {
        packages = with pkgs; [
          cargo
          rustc
          rust-analyzer
          clippy
          rustfmt
        ];
      };

      devShells.waybar-org-clock = pkgs.mkShell {
        packages = with pkgs; [
          cargo
          rustc
          rust-analyzer
          clippy
          rustfmt
        ];
      };

      devShells.default = pkgs.mkShell {
        name = "nixos-unified-template-shell";
        meta.description = "Shell environment for modifying this Nix configuration";
        packages =
          with pkgs;
          [
            self'.formatter
            rsync
            s3cmd
            just
            nixd
            nixfmt
            dnscontrol
            # (terraform_1.withPlugins (p: with p; [ dmacvicar_libvirt ])) # too expensive to build
            cloud-init
            ansible
            ansible-lint
            # For check-arion-images script
            jq
            curl
            # For ncf tool
            ncf
            ncfPythonEnv
            # For monitoring dashboards
            monitoring
            monitoringPythonEnv
            grafanactl
            inputs'.clan-core.packages.clan-cli
          ]
          ++ ncfRuntimeDeps;
        shellHook = ''
          export PYTHONPATH="$PWD/tools/ncf:$PWD/monitoring:$PYTHONPATH"
        '';
      };
    };
}
