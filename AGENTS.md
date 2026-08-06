General
=======

- Never search for source code in /nix/store/ without permission. Also,
  a lot of nix-related source-code is checked out in ../nix/ subdirs
  (nixpkgs, home-manager, system-manager, clan, ...)

- This is nix flake - if adding/removing new files, make nix aware of
  their existence by `git add --intent-to-add`/`git rm`. After that
  everything is auto-wired.

- This flake follows dendritic pattern, where most of .nix files are a
  flake-parts modules, exporting other types of modules, such as
  nixos/home-manager/system-manager modules.

- This flakes uses ~flake-file~, so changing `flake.nix` directly is
  not allowed. To change dependecies, the underlying .nix-files should
  be modified, and `nix run .#write-flake` should be executed. (One
  common caveat when adding new flake inputs - new flake-parts modules
  can't be used until `flake.nix` regen; in that case 2-step process
  is necessary: first add input/regen, then use the flake-module(s)).

Emacs
=====

If you need to make emacs-related changes, read ./docs/EMACS.org

Allowed modifications
=====================

~AGENTS.md~ and files in ./docs are off limits, only humans can change
them.

Planning and progress tracking
==============================

Plan/progress .org documents live in todo/ directory. These are live
dynamic documents, so all planning/debugging/discovery/progress
information should be added/updated reasonably often.
