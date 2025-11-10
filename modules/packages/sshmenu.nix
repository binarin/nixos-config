{ self, ... }:
{
  perSystem =
    { pkgs, lib, ... }:
    {
      packages.sshmenu = pkgs.writeTextFile {
        name = "sshmenu";
        destination = "/bin/sshmenu";
        executable = true;
        text = ''
          #!${lib.getExe pkgs.zsh}
          export PATH="${
            lib.makeBinPath [
              pkgs.rxvt-unicode
              pkgs.xxd
            ]
          }:$PATH"
        ''
        + (builtins.readFile "${self}/files/sshmenu");
      };
    };
}
