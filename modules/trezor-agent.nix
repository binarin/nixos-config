{
  inputs,
  ...
}:
let
  mkTrezorAgentPkgs =
    system:
    import inputs.nixpkgs-trezor-agent {
      inherit system;
      config = {
        allowUnfree = true;
        permittedInsecurePackages = [
          # used by trezor-agent, but vulnerability is about leaking
          # generated keys - so doesn't matter, as keys do not leave
          # trezor
          "python3.12-ecdsa-0.19.1"
          "python3.13-ecdsa-0.19.1"
        ];
      };
    };
in
{
  flake-file.inputs.nixpkgs-trezor-agent = {
    url = "github:nixos/nixpkgs?rev=41e216c0ca66c83b12ab7a98cc326b5db01db646";
  };

  # Expose trezor-agent as a package so CI can test building it
  perSystem =
    { system, ... }:
    {
      packages.trezor-agent = (mkTrezorAgentPkgs system).trezor-agent;
    };

  flake.homeModules.trezor-agent =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.trezor-agent";
      home.packages = [
        (mkTrezorAgentPkgs pkgs.stdenv.hostPlatform.system).trezor-agent
      ];
    };
}
