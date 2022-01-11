{
  ishamael = {
    description = "Dell Precision 5560";
    lan = {
      mac = "ec:f4:bb:5b:9a:84";
      ip = "192.168.2.12";
    };
    wifi = {
      mac = "f8:16:54:1c:95:c6";
      ip = "192.168.2.10";
    };
  };
  kodi = {
    description = "Acer Aspire Revo";
    lan = {
      mac = "90:fb:a6:2b:02:33";
      ip = "192.168.2.7";
      iface = "enp0s10";
    };
  };
  lanfear = {
    description = "Gaming PC";
    lan = {
      mac = "d8:50:e6:bf:91:63";
      ip = "192.168.2.4";
    };
  };
  amon = rec {
    description = "HP Proliant MicroServer";
    lan = {
      mac = "3c:d9:2b:0c:1d:e0";
      ip = "192.168.2.11";
    };
    ipmi = {
      ip = "192.168.2.27";
      mac = "44:1e:a1:3d:fa:15";
    };
    deployTarget = {
      default = "amon.binarin.ru";
      home = lan.ip;
    };
  };
  h315 = {
    wan = {
      mac = "48:db:50:be:e7:fb";
      ip = "192.168.2.13";
    };
    lan = {
      ip = "192.168.3.13";
    };
  };
  rpi3 = {
    description = "My first rPi";
    wlan = {
      mac = "b8:27:eb:83:a1:45";
      ip = "192.168.2.14";
    };
    lan = {
      mac = "b8:27:eb:d6:f4:10";
      ip = "192.168.2.15";
    };
  };
  epson = {
    description = "epson MFD";
    wlan = {
      ip = "192.168.2.16";
      mac = "64:eb:8c:91:b2:2f";
    };
  };
  dir632 = {
    description = "D-Link DIR-632 router";
    wlan = {
      ip = "192.168.2.17";
      mac = "00:11:22:33:44:55";
    };
  };
  marax = {
    description = "Old Asus laptop (ul20ft)";
    wlan = {
      ip = "192.168.2.18";
      mac = "74:f0:6d:92:3b:cf";
    };
    lan = {
      ip = "192.168.2.22";
      mac = "20:cf:30:53:4d:e5";
    };
  };
  unify-cloud-key = {
    lan = {
      ip = "192.168.2.19";
    };
  };
  balthamel = {
    description = "e7270 workstation";
    known-as = {
      amon-ovpn = "10.10.10.18";
    };
    sshKeys = {
      binarin = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQClOg+Rcj3dcGZRWzsGJ+KLlW8oQUaXZ6zXdAc8VZotzRj2agj9klYPmTiCXgfKHsPTFBosJntzLoneEMDHNE1RU2/3q7dkcytOea1kE25g0NNv9n6ZwWQMjHRDeyEhsd6BJn1a+Kx9jPWEGX+FZd2hAxph+0tU8M6m9NdkHz2xtT4+/XTjtRCNaZ6t59RdOX5mCxyl/911jq+kBb5QZot6pz9UybPPVfLvIsqMvzNUX+N871XsdiaLegKyGEyBiWW4ni5vHTj7cE1n/ykJlqUrUzvcu6A1acE05tcgK8WBfY+sOseFG5Sse87uICAmaO3fPV5Esi0bPG1b4kn+0MuT balthamel (booking ws), nixos - 2017-09-28";
      root = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJLK34Ge9j6oabaCk9cuGyGk8oZvbN9t0tXuoaKEzo39f6n2Sef4Na8hT9pFrcDR34esmc5byh6uqAuZIzwjyh3+OQe6TDbzjHEYGnMBjXSKaEKxRna8BFroc0saYG8/KHyc0A6kTEF72LJ0iDcYxqrzVO1TzFpgaB4vVNUu2GiWoYHqUWA2DcOQBcyBiyjRRThXRs28ISN+lAoexQzAL20KnvJ5GmJ9anelOmIdfHxitcdmu/goxFjml6CgYoFtIY4Oo711rWErXmjmFuHIDiu0M+ufLZtj/dl153qMi9BGLY9HH3LHkd7FZjGWIwA2TLdKO+L6Ra8AVePIe3G4KL 20180926 - balthamel (root)";
    };
  };
  naberius = {
    description = "hetzner host";
  };
  philips-hue-bridge = {
    lan = {
      ip = "192.168.2.20";
    };
  };

  rpi3b_plus = {
    description = "octopi";
    lan = {
      mac = "b8:27:eb:72:bd:04";
      ip = "192.168.2.21";
    };
    wan = {
      mac = "b8:27:eb:27:e8:51";
      ip = "192.168.2.24";
    };
  };

  hass-vm = {
    lan = {
      ip = "192.168.2.23";
      mac = "52:54:00:e6:fc:25";
    };
  };

  barbatos = {
    description = "T5500";
    hostId = "aafbe1f4";

    lan = {
      mac = "18:03:73:21:e7:6d";
      ip = "192.168.2.25";
    };

    pihole = {
      ip = "192.168.2.28";
    };

  };

  pi4-kodi = {
    wan = {
      ip = "192.168.2.29";
      mac = "dc:a6:32:f0:c6:02";
    };
    lan = {
      mac = "dc:a6:32:f0:c6:01";
      ip = "192.168.2.30";
    };
  };

  valak = {
    hostId = "55f9cd65";
    description = "vfio rig";
    lan = {
      mac = "70:85:c2:f4:6b:70";
      ip = "192.168.2.26";
    };
  };

  usg = {
    description = "UniFi Security Gateway";
    lan = {
      ip = "192.168.2.1";
    };
    searchDomains = ["localdomain"];
    domain = "localdomain";
  };

  nix-builder = {
    lan = {
      ip = "192.168.2.31";
    };
  };

  debian-11-rmq = {
    lan = {
      ip = "192.168.2.32";
      mac = "94:A7:B7:FB:EB:4F";
    };
  };

  k0s-1 = {
    lan = {
      ip = "192.168.2.33";
    };
  };

  k0s-2 = {
    lan = {
      ip = "192.168.2.34";
    };
  };

  k0s-3 = {
    lan = {
      ip = "192.168.2.35";
    };
  };

  haproxy = {
    lan = {
      ip = "192.168.2.36";
    };
  };

  guacamole = {
    lan = {
      ip = "192.168.2.37";
    };
  };

  zapustos = {
    lan = {
      ip = "192.168.2.38";
    };
  };
}
