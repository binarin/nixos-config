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
          dnscontrol
          (terraform_1.withPlugins (p: with p; [ libvirt ]))
          cloud-init
        ];
      };
    };
}
