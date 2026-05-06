{
  lib,
  stdenv,
  fetchurl,
  curl,
  openssl,
  zlib-ng,
  expat,
  perl,
  python3,
  gettext,
  gnugrep,
  gnused,
  gawk,
  coreutils,
  openssh,
  pcre2,
  makeWrapper,
  libiconv,
  asciidoc,
  texinfo,
  xmlto,
  docbook2x,
  docbook_xsl,
  docbook_xml_dtd_45,
  libxslt,
  tcl,
  tk,
  pkg-config,

  # Optional features
  perlSupport ? true,
  nlsSupport ? true,
  guiSupport ? false,
  pythonSupport ? true,
  withpcre2 ? true,
  withManual ? false,
  withSsh ? false,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "git";
  version = "2.54.0";

  src = fetchurl {
    url = "https://www.kernel.org/pub/software/scm/git/git-${finalAttrs.version}.tar.xz";
    hash = "sha256-9okWI2TBDeee+Jqo2/SHMesFfjTtu9IKylEM4BVGgaM=";
  };

  outputs = [ "out" ] ++ lib.optional withManual "doc";
  separateDebugInfo = true;

  enableParallelBuilding = true;
  enableParallelInstalling = true;

  nativeBuildInputs = [
    gettext
    perl
    makeWrapper
    pkg-config
  ] ++ lib.optionals withManual [
    asciidoc
    texinfo
    xmlto
    docbook2x
    docbook_xsl
    docbook_xml_dtd_45
    libxslt
  ];

  buildInputs = [
    curl
    openssl
    zlib-ng
    expat
    libiconv
  ] ++ lib.optionals perlSupport [ perl ]
  ++ lib.optionals guiSupport [ tcl tk ]
  ++ lib.optionals withpcre2 [ pcre2 ];

  makeFlags = [
    "prefix=\${out}"
    "ZLIB_NG=1"
    "SHELL_PATH=${stdenv.shell}"
  ] ++ (if perlSupport then [ "PERL_PATH=${perl}/bin/perl" ] else [ "NO_PERL=1" ])
  ++ (if pythonSupport then [ "PYTHON_PATH=${python3}/bin/python" ] else [ "NO_PYTHON=1" ])
  ++ lib.optional withpcre2 "USE_LIBPCRE2=1"
  ++ lib.optional (!nlsSupport) "NO_GETTEXT=1"
  ++ lib.optional (!withManual) "NO_INSTALL_HARDLINKS=1";

  preBuild = ''
    makeFlagsArray+=( perllibdir=$out/$(perl -MConfig -wle 'print substr $Config{installsitelib}, 1 + length $Config{siteprefixexp}') )
  '';

  postInstall = ''
    # grep is a runtime dependency
    substituteInPlace $out/libexec/git-core/git-sh-setup \
        --replace ' grep' ' ${gnugrep}/bin/grep' \
        --replace ' egrep' ' ${gnugrep}/bin/egrep'

    # Fix references to coreutil binaries in shell scripts
    SCRIPT="$(cat <<'EOS'
      BEGIN{
        @a=(
          '${gnugrep}/bin/grep', '${gnused}/bin/sed', '${gawk}/bin/awk',
          '${coreutils}/bin/cut', '${coreutils}/bin/basename', '${coreutils}/bin/dirname',
          '${coreutils}/bin/wc', '${coreutils}/bin/tr'
          ${lib.optionalString perlSupport ", '${perl}/bin/perl'"}
        );
      }
      foreach $c (@a) {
        $n=(split("/", $c))[-1];
        s|(?<=[^#][^/.-])\b''${n}(?=\s)|''${c}|g
      }
    EOS
    )"
    perl -0777 -i -pe "$SCRIPT" \
      $out/libexec/git-core/git-{sh-setup,filter-branch,merge-octopus,mergetool,quiltimport,request-pull,submodule,subtree,web--browse}

    ln -s $out/libexec/git-core/git-http-backend $out/bin/git-http-backend
  ''
  + lib.optionalString (!guiSupport) ''
    for prog in bin/gitk libexec/git-core/git-gui; do
      rm -f "$out/$prog"
    done
  ''
  + lib.optionalString guiSupport ''
    for prog in bin/gitk libexec/git-core/{git-gui,git-citool,git-gui--askpass}; do
      sed -i -e "s|exec 'wish'|exec '${tk}/bin/wish'|g" \
             -e "s|exec wish|exec '${tk}/bin/wish'|g" \
             "$out/$prog"
    done
  '';

  doCheck = false;

  meta = with lib; {
    description = "Distributed version control system";
    homepage = "https://git-scm.com/";
    license = licenses.gpl2;
    platforms = platforms.all;
    mainProgram = "git";
  };
})
