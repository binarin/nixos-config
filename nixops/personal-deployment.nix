let
  hosts = import ./personal-hosts.nix;
in {
  kodi = {
    deployment.targetEnv = "none";
    deployment.targetHost = hosts.kodi.lan.ip;
    deployment.targetPort = 22;
  };
  amon = {
    deployment.targetEnv = "none";
    deployment.targetHost = hosts.amon.lan.ip;
    deployment.targetPort = 22;
  };
}
