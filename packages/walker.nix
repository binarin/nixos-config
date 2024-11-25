{
  pkgs,
  fetchFromGitHub,
  ...
}:
pkgs.bleeding.walker.overrideAttrs (
  finalAttrs: prevAttrs: {
    version = "0.9.7";

    src = fetchFromGitHub {
      owner = "abenz1267";
      repo = "walker";
      rev = "v${finalAttrs.version}";
      hash = "sha256-LxN1J38OH2kSDRmQHCB++UjL92JKcbuZnJOTkFImHxA=";
    };

    vendorHash = "sha256-nc/WKBhUxhs1aNUg/GM7vhrKd7FrUdl2uKp7MX2VCdE=";
  }
)
