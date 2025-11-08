{
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.hostConfig.feature.interactive-cli {
    nixpkgs.overlays = [
      # (final: prev: {
      #   trezor-agent = (prev.python3Packages.trezor-agent.override {libagent = libagent;}).overrideAttrs (finalAttrs: prevAttrs: {
      #     version = "0.15.0-dev-2024-12-25";
      #     name = "${prevAttrs.pname}-${finalAttrs.version}";
      #     src = trezorSrc;
      #     sourceRoot = "${finalAttrs.src.name}/agents/trezor";
      #   });
      # })
    ];
  };
}
