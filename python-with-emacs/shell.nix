let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  name = "some-shell";
  buildInputs = [
    (pkgs.poetry2nix.mkPoetryEnv {
      projectDir = ./.;
    })
  ];
}
