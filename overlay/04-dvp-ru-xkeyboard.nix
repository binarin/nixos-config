self: super: {
  xorg = super.xorg // {
    xkeyboardconfig = super.pkgs.lib.overrideDerivation super.xorg.xkeyboardconfig (old: {
      patches = [ ../roles/xkb.patch ];
    });
  };
}
