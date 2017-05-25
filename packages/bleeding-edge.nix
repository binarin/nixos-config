{config, ...}:
let nixpkgs = import ../nixpkgs-master {inherit (config.nixpkgs) config;};
in
{
  config = {
    nixpkgs.config.packageOverrides = super: {
        bleeding = nixpkgs.pkgs;
    };
  };
}
