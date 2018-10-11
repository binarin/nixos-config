self: super: {
  xorg = super.xorg // rec {
    xkeyboardconfig_dvp = super.pkgs.lib.overrideDerivation super.xorg.xkeyboardconfig (old: {
      patches = [ ../roles/xkb.patch ];
    });
    # xorgserver = super.pkgs.lib.overrideDerivation super.xorg.xorgserver (old: {
    #   configureFlags = [
    #     "--enable-kdrive"             # not built by default
    #     "--enable-xephyr"
    #     "--enable-xcsecurity"         # enable SECURITY extension
    #     "--with-default-font-path="   # there were only paths containing "${prefix}",
    #                                   # and there are no fonts in this package anyway
    #     "--with-xkb-bin-directory=${xorg.xkbcomp}/bin"
    #     "--with-xkb-path=${xorg.xkeyboardconfig}/share/X11/xkb"
    #     "--with-xkb-output=$out/share/X11/xkb/compiled"
    #     "--enable-glamor"
    #   ];
    # });
    # setxkbmap = super.pkgs.lib.overrideDerivation super.xorg.setxkbmap (old: {
    #   postInstall =
    #     ''
    #       mkdir -p $out/share
    #       ln -sfn ${xkeyboard_config_dvp}/etc/X11 $out/share/X11
    #     '';
    # });
    # xkbcomp = super.pkgs.lib.overrideDerivation super.xorg.xkbcomp (old: {
    #   configureFlags = "--with-xkb-config-root=${xkeyboard_config_dvp}/share/X11/xkb";
    # });
  };
}
