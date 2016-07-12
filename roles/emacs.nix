{config, pkgs, fetchFromGitHub, nixpkgs ? null,...}:

fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  sha256 = "1fybicg46fc5jjqv7g2d3dnj1x9n58m2fg9x6qxn9l8qlzk9yxkq";
  ref = "7060eaf0bb98f3b4b6d10e7e55d20df9527866bf";
}
