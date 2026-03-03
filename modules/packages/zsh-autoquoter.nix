{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.zsh-autoquoter = pkgs.stdenv.mkDerivation {
        pname = "zsh-autoquoter";
        version = "unstable-2022-01-15";

        src = pkgs.fetchFromGitHub {
          owner = "ianthehenry";
          repo = "zsh-autoquoter";
          rev = "9e3b1b216bf7b61a9807a242bae730b5fc232a44";
          hash = "sha256-CdyKIGxOnWGWPeBuNz067zp8/a394H0Ec2h3CA3oIx0=";
        };

        installPhase = ''
          install -D zsh-autoquoter.zsh $out/share/zsh/zsh-autoquoter/zsh-autoquoter.zsh
        '';

        meta = {
          homepage = "https://github.com/ianthehenry/zsh-autoquoter";
          description = "Automatically quote shell command arguments";
          license = pkgs.lib.licenses.mit;
          platforms = pkgs.lib.platforms.all;
        };
      };
    };
}
