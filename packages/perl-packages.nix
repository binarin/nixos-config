{pkgs, ...}:

with pkgs;
let
  PathTiny = buildPerlPackage rec {
    name = "Path-Tiny-0.104";
    src = fetchurl {
      url = "mirror://cpan/authors/id/D/DA/DAGOLDEN/${name}.tar.gz";
      sha256 = "c69f1dcfeb4aa004086deb9bc14c7d79f45798b947f1efbd634a3442e267aaef";
    };
    meta = {
      homepage = https://github.com/dagolden/Path-Tiny;
      description = "File path utility";
      license = stdenv.lib.licenses.asl20;
    };
  };
  PerlTags = buildPerlPackage rec {
    name = "Perl-Tags-0.32";
    src = fetchurl {
      url = "mirror://cpan/authors/id/O/OS/OSFAMERON/${name}.tar.gz";
      sha256 = "0230551d9379f857f596c149190c5f35422dfd6df24ce104ec070c287daf5741";
    };
    propagatedBuildInputs = [ PathTiny ];
    meta = {
    };
  };

in {
  options = {};
  config = {
    environment.systemPackages = [ PerlTags ];
  };
}
