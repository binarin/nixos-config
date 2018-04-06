{pkgs, stdenv, ...}:

let
  packageOverrides = self: super: {
    QSTK = super.buildPythonPackage rec {
      name = "QSTK-${version}";
      version = "0.2.8";
      propagatedBuildInputs = with self; [ dateutil numpy scipy matplotlib pandas cvxopt scikitlearn ];
      src = pkgs.fetchFromGitHub {
        owner = "binarin";
        repo = "QuantSoftwareToolkit";
        rev = "a48a80465840829cc1b294e73433eecf35fdd9be";
        sha256 = "1q7n3gr1inm86q9xb5g8mm1gzn6k8rcm7sk0qy8631lk4a66nqbb";
      };
    };
  };
  python = pkgs.python27.override {inherit packageOverrides; };
  packagesToInstall = ps: with ps; [
    # QSTK
    ipython
    jupyter
  ];
in {
  options = {};
  config = {
    environment.systemPackages = [ pkgs.myPython27 ];
    nixpkgs.config.packageOverrides = super: rec {
      myPython27 = python.buildEnv.override {
        extraLibs = packagesToInstall python.passthru.pkgs;
        ignoreCollisions = true;
      };
    };
  };
}

