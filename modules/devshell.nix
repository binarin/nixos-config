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
          (terraform_1.withPlugins (p: with p; [ dmacvicar_libvirt ]))
          cloud-init
          ansible
          ansible-lint
          # For check-arion-images script
          jq
          curl
        ];
      };
    };
}
