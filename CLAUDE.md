This is my personal infra:
- NixOS configurations
- Ansible for non-NixOS machines (but some data is being consumed from NixOS configurations)
- Terraform for creating non-NixOS machines

Git
===
Before doing any changes, check that everything is committed. If not -
start a copy of yourself to analyze current uncommitted changes, and
do one or more commits, trying to group logically related changes.

ToDo list
=========

NixOS
=====

Dendritic
---------

I'm trying to restructure this repo to use dendritic pattern: where
all classes of modules live in the same file
(i.e. modules/flake-parts/something.nix instead of
modules/{home,nixos,shared}/something.nix). Some modules with suffix
`-new` are artifacts of this transition, and should eventually go away.


Inventory
---------

NixOS configuration is primary source of inventory, in inventory/ -
main parts are SSH public keys and IP allocation. Ideally, it should
be the only source of truth - but for now there are some IP addresses
hardcoded here and there.

Ansible
=======


Terraform
=========


SSH security
============
