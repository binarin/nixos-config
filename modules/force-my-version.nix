# Get version strings from my config repository and enforce them everywhere.
# For several reasons
# - I use several different nixpkgs checkout, so only 'nixos-config' commit id truly represents the system source
# - commitIdFromGitRepo doesn't work with git submodules
# - enforce truly repeatable builds of system by having just a checkout of my 'nixos-config', otherwise small changes creep into files like /etc/os-release

{pkgs, lib, ...}:

let
  gitRepo      = "${toString pkgs.path}/../.git";
  gitCommitId  = lib.substring 0 7 (lib.commitIdFromGitRepo gitRepo);
in {
  system.nixos.versionSuffix = "-binarin-${gitCommitId}";
  system.nixos.label = "binarin-${gitCommitId}";
}
