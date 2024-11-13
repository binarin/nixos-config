{ flake, config, ... }:

let
  inherit (flake) inputs;
  inherit (inputs) self;
in
{

  imports = [
    (self + "/packages/standard-linux-tools.nix")
  ];

  config = lib.mkIf config.hostConfig.feature.server {
    time.timeZone = "Europe/Amsterdam";
    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = "yes";
    services.fail2ban.enable = true;
    networking.firewall.allowedTCPPorts = [
      22
    ];

    nixpkgs.config = {
      allowUnfree = true;
      oraclejdk.accept_license = true;
    };

    networking.firewall.enable = true;

    environment.enableAllTerminfo = true;
  };

}
