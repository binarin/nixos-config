{
  perSystem =
    { pkgs, ... }:
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
          (python3.withPackages (ps: with ps; [
            typer
            rich
            ruamel-yaml
            pydantic
            gitpython
          ]))
        ];
      };
    };
}
