{
  perSystem =
    { pkgs, ... }:
    let
      ncf-python = pkgs.python3.withPackages (
        ps: with ps; [
          typer
          rich
          ruamel-yaml
          pydantic
          gitpython
        ]
      );
      ncf = pkgs.writeShellScriptBin "ncf" ''
        exec ${ncf-python}/bin/python -m ncf "$@"
      '';
    in
    {
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
        ];
        shellHook = ''
          export PYTHONPATH="$PWD/tools/ncf:$PYTHONPATH"
        '';
      };
    };
}
