{pkgs, config, lib, ...}:
let
  trezorSrc = pkgs.fetchFromGitHub {
    owner = "romanz";
    repo = "trezor-agent";
    rev = "e8e033fb0bd6e985ecf49a802f490dbd971a1676";
    hash = "sha256-A3WdhiAarTCJLWJEvwN8/PwMtiYLUYlUcuqJrGobeFc=";
  };
  libagent = pkgs.python3Packages.libagent.overrideAttrs (finalAttrs: prevAttrs: {
    version = "0.15.0-dev-2024-12-25";
    name = "${prevAttrs.pname}-${finalAttrs.version}";
    src = trezorSrc;
  });
in {
  config = lib.mkIf config.hostConfig.feature.interactive-cli {
    nixpkgs.overlays = [
      (final: prev: {
        trezor-agent = (prev.python3Packages.trezor-agent.override {libagent = libagent;}).overrideAttrs (finalAttrs: prevAttrs: {
          version = "0.15.0-dev-2024-12-25";
          name = "${prevAttrs.pname}-${finalAttrs.version}";
          src = trezorSrc;
          sourceRoot = "${finalAttrs.src.name}/agents/trezor";
        });
      })
    ];
  };
}
