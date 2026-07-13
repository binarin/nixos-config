# Local replacement for the upstream `overlays.shared-nixpkgs` overlay that
# llm-agents.nix no longer exposes.
#
# We `callPackage` directly into the llm-agents source tree for only the
# packages we actually consume, reconstructing the minimal `perSystem.self`
# set each one needs to resolve sibling references.
{ inputs, ... }:
let
  flake = inputs.nix-ai-tools;
  src = flake;

  # Names of the packages we pull in. Each may reference siblings via
  # `perSystem.self.<name>`, so we evaluate them as a fixed point.
  packageNames = [
    "buildNpmPackage"
    "versionCheckHomeHook"
    "wrapBuddy"
    "claude-code"
    "claude-code-router"
    "pi"
    "workmux"
  ];

  # callPackage a single llm-agents package, threading in the same arguments
  # the upstream flake does (`flake`, `inputs`, `perSystem.self`, `system`).
  callPackage =
    final: perSystemSelf: name:
    let
      fn = import (src + "/packages/${name}/default.nix");
      fnArgs = builtins.functionArgs fn;
      supplied = {
        pkgs = final;
        perSystem.self = perSystemSelf;
        inherit flake inputs;
        system = final.stdenv.hostPlatform.system;
      };
    in
    final.callPackage fn (builtins.intersectAttrs fnArgs supplied);
in
{
  flake.overlays.llm-agents = final: _prev:
    let
      perSystemSelf = final.lib.fix (
        self: final.lib.genAttrs packageNames (callPackage final self)
      );
    in
    {
      llm-agents = perSystemSelf;
    };
}
