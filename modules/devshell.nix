{
  perSystem =
    { pkgs, ... }:
    let
      ncf = pkgs.python3.pkgs.buildPythonApplication {
        pname = "ncf";
        version = "0.1.0";
        format = "pyproject";

        src = ../tools/ncf;

        build-system = [ pkgs.python3.pkgs.setuptools ];

        dependencies = with pkgs.python3.pkgs; [
          typer
          rich
          ruamel-yaml
          pydantic
          gitpython
          jinja2
          tomlkit
        ];
      };
    in
    {
      packages.ncf = ncf;

      devShells.default = pkgs.mkShell {
        name = "nixos-unified-template-shell";
        meta.description = "Shell environment for modifying this Nix configuration";
        packages = with pkgs; [
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
          ssh-to-age
          ncf
          nix-output-monitor # nom for nicer build output
          # For inject-iso-wifi.sh script
          libarchive # provides bsdtar
          fakeroot
          squashfsTools # provides unsquashfs/mksquashfs
          xorriso
        ];
        shellHook = ''
          export PYTHONPATH="$PWD/tools/ncf:$PYTHONPATH"
        '';
      };
    };
}
