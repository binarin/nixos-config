{
  perSystem =
    { pkgs, ... }:
    let
      nixos-secrets-python = pkgs.python3.withPackages (ps: with ps; [
        typer
        rich
        ruamel-yaml
        pydantic
        gitpython
      ]);
      nixos-secrets = pkgs.writeShellScriptBin "nixos-secrets" ''
        exec ${nixos-secrets-python}/bin/python -m nixos_secrets "$@"
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
          # For nixos-secrets tool
          ssh-to-age
          nixos-secrets
        ];
        shellHook = ''
          export PYTHONPATH="$PWD/tools/nixos-secrets:$PYTHONPATH"
        '';
      };
    };
}
