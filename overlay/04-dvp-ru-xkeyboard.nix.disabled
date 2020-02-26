self: super: rec {
  xkbvalidate = super.xkbvalidate.override { libxkbcommon = libxkbcommon_dvp; };
  libxkbcommon_dvp = super.pkgs.lib.overrideDerivation super.libxkbcommon (oldAttrs: {
    mesonFlags = [
      "-Denable-wayland=false"
      "-Dxkb-config-root=${xorg.xkeyboardconfig_dvp}/etc/X11/xkb"
      "-Dx-locale-root=${super.xorg.libX11.out}/share/X11/locale"
    ];
  });
  xorg = super.xorg // rec {
    xkeyboardconfig_dvp = super.pkgs.lib.overrideDerivation super.xorg.xkeyboardconfig (old: {
      patches = [ ../profile/xkb.patch ];
    });
    xorgserver = super.xorg.xorgserver.overrideAttrs (old: {
      configureFlags = [
        "--enable-kdrive"             # not built by default
        "--enable-xephyr"
        "--enable-xcsecurity"         # enable SECURITY extension
        "--with-default-font-path="   # there were only paths containing "${prefix}",
                                      # and there are no fonts in this package anyway
        "--with-xkb-bin-directory=${xkbcomp_dvp}/bin"
        "--with-xkb-path=${xkeyboardconfig_dvp}/share/X11/xkb"
        "--with-xkb-output=$out/share/X11/xkb/compiled"
        "--enable-glamor"
      ];
    });
    setxkbmap = super.pkgs.lib.overrideDerivation super.xorg.setxkbmap (old: {
      postInstall =
        ''
          mkdir -p $out/share
          ln -sfn ${xkeyboardconfig_dvp}/etc/X11 $out/share/X11
        '';
    });
    xkbcomp_dvp = super.pkgs.lib.overrideDerivation super.xorg.xkbcomp (old: {
      configureFlags = "--with-xkb-config-root=${xkeyboardconfig_dvp}/etc/X11/xkb";
    });
  };
}
