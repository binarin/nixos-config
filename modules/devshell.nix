{
  perSystem =
    { pkgs, ... }:
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

      ncfRuntimeDeps = with pkgs; [
        nix
        nix-output-monitor # nom for nicer build output
        git
        git-crypt
        openssh # ssh-keygen
        age # age-keygen
        sops
        ssh-to-age
        yamlfmt
        apg
        deploy-rs # For ncf deploy commands
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
        ];

        postFixup = ''
          wrapProgram $out/bin/ncf \
            --prefix PATH : ${pkgs.lib.makeBinPath ncfRuntimeDeps}
        '';
      };

      ncfPythonEnv = pkgs.python3.withPackages (ps: ncf.dependencies ++ [ ps.pytest ]);

    in
    {
      packages.ncf = ncf;
      packages.env-aware-nix-run = env-aware-nix-run;
      packages.env-aware-nix-develop = env-aware-nix-develop;

      devShells.default = pkgs.mkShell {
        name = "nixos-unified-template-shell";
        meta.description = "Shell environment for modifying this Nix configuration";
        packages =
          with pkgs;
          [
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
          ]
          ++ ncfRuntimeDeps;
        shellHook = ''
          export PYTHONPATH="$PWD/tools/ncf:$PYTHONPATH"
        '';
      };
    };
}
