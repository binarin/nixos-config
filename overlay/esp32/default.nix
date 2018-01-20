self: super: let
inherit (super) lib stdenv;
in {
  xtensa-esp32-toolchain = stdenv.mkDerivation rec {
    name = "xtensa-esp32-toolchain";
    src = super.fetchurl {
      url = https://dl.espressif.com/dl/xtensa-esp32-elf-linux64-1.22.0-80-g6c4433a-5.2.0.tar.gz;
      sha256 = "0mji8jq1dg198z8bl50i0hs3drdqa446kvf6xpjx9ha63lanrs9z";
    };
    rpath = stdenv.lib.makeLibraryPath (with self; [
      ncurses5
      python
      zlib
    ]) + ":${stdenv.cc.cc.lib}/lib64";
    buildCommand = ''
      tar xzf $src
      mv xtensa-esp32-elf $out
      for file in $(find $out -executable -type f); do
        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $file || true
        patchelf --set-rpath ${rpath}:\$ORIGIN $file || true
      done
      $fixupPhase
    '';
  };
  crosstool-NG = stdenv.mkDerivation {
    name = "crosstool-NG";
    src = super.fetchFromGitHub {
      owner = "espressif";
      repo = "crosstool-NG";
      rev = "6c4433a51e4f2f2f9d9d4a13e75cd951acdfa80c"; # 'xtensa-1.22.x' branch
      sha256 = "03qg9vb0mf10nfslggmb7lc426l0gxqhfyvbadh86x41n2j6ddg6";
    };
    tarballs = import ./tarballs.nix { fetchurl = super.fetchurl; };
    buildInputs = with self; [ autoconf git which gperf bison flex texinfo wget help2man libtool automake ncurses python27 ];
    configurePhase = ''true'';
    hardeningDisable = [ "all" ];
    buildPhase = ''
      ./bootstrap
      ./configure --enable-local
      make install

      ./ct-ng xtensa-esp32-elf

      mkdir -p .build/tarballs
      for sourceArchive in $tarballs; do
        ln -s $sourceArchive .build/tarballs/$(basename $sourceArchive | cut -c 34-)
      done
      ls -la .build/tarballs

      ./ct-ng build
    '';
    installPhase = ''true'';
  };
  esp-idf = stdenv.mkDerivation {
    name = "esp-idf";
    buildInputs = [ self.xtensa-esp32-toolchain ];
  };
}
