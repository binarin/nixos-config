let
  hosts = import ./personal-hosts.nix;
  deployTarget = host: hosts."${host}".deployTarget.default;
    # weAreAt = builtins.extraBuiltins.detectEnv; in
    # if builtins.hasAttr weAreAt hosts."$host".deployTarget
    # then hosts."$host".deployTarget."$weAreAt"
    # else
in {
  balthamel = {
    deployment.targetEnv = "none";
    deployment.targetHost = if builtins.extraBuiltins.hostname == "balthamel"
                            then "localhost"
                            else hosts.balthamel.known-as.amon-ovpn;
    deployment.targetPort = 22;
  };
  kodi = {
    deployment.targetEnv = "none";
    deployment.targetHost = hosts.kodi.lan.ip;
    deployment.targetPort = 22;
  };
  amon = {
    deployment.targetEnv = "none";
    deployment.targetHost = deployTarget "amon";
    deployment.targetPort = 22;
  };
  naberius = {
    deployment.targetEnv = "hetzner";
    deployment.hetzner.mainIPv4 = "5.9.106.77";
    deployment.hetzner.partitions = ''
        clearpart --all --initlabel --drives=sda,sdb
        part swap1 --recommended --label=swap1 --fstype=swap --ondisk=sda
        part swap2 --recommended --label=swap2 --fstype=swap --ondisk=sdb
        part raid.0a --size=1024 --ondisk=sda
        part raid.0b --size=1024 --ondisk=sdb
        part raid.1a --grow --ondisk=sda
        part raid.1b --grow --ondisk=sdb
        raid /boot --level=1 --device=boot --fstype=ext2 --label=boot raid.0a raid.0b
        raid pv.0 --level=1 --device=pv.0 raid.1a raid.1b
        volgroup sysvg pv.0
        logvol / --vgname=sysvg --size=65536 --fstype=ext4 --name=root --label=root
    '';
  };
}
