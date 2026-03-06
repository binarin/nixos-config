{
  perSystem =
    { pkgs, ... }:
    let
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
