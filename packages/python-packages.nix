{pkgs, fetchurl, ...}:

let
  packageOverrides = self: super: {
    QSTK = super.buildPythonPackage rec {
      name = "QSTK-${version}";
      version = "0.2.8";
      propagatedBuildInputs = with self; [ dateutil numpy scipy matplotlib pandas cvxopt scikitlearn ];
      src = pkgs.fetchurl {
        name = "QSTK-0.2.8.tar.gz";
        url = "https://pypi.python.org/packages/8c/15/3298c4ee5d187a462883a7f80d7621a05e8b880a8234729e733769a3476f/QSTK-0.2.8.tar.gz#md5=bac816724bcb6eebd5dbb09e43166815";
        sha256 = "1h88m8ahk2c0mqgclryax0sw9wd3lgfx0vly002chw60xb0zvbrz";
      };
    };
    dateutil = super.buildPythonPackage {
      name = "python-dateutil-1.5";
      src = pkgs.fetchurl {
        url = "https://pypi.python.org/packages/b4/7c/df59c89a753eb33c7c44e1dd42de0e9bc2ccdd5a4d576e0bfad97cc280cb/python-dateutil-1.5.tar.gz";
        sha256 = "02dhw57jf5kjcp7ng1if7vdrbnlpb9yjmz7wygwwvf3gni4766bg";
      };
    };
  };
in {
  options = {};
  config = {
    environment.systemPackages = [ pkgs.myPython27 ];
    nixpkgs.config.packageOverrides = super: rec {
      myPython27 = (pkgs.python27.override {inherit packageOverrides;}).withPackages (ps: with ps; [
        QSTK
      ]);
    };
  };
}
