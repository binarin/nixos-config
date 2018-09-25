{pkgs, config, ...}:

{
  environment.systemPackages = with pkgs; [
    nixops
  ];

  nix.extraOptions = ''
    plugin-files = ${pkgs.nix-plugins_4.override { nix = config.nix.package; }}/lib/nix/plugins/libnix-extra-builtins.so
  '';

}
