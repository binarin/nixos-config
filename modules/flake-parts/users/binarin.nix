{ self, lib, ... }:
let
  expandFile = home: fn: if lib.isAttrs fn
                   then fn // { file = "${home}/${fn.file}"; }
                   else "${home}/${fn}";
  expandDir = home: fn: if lib.isAttrs fn
                  then fn // { directory = "${home}/${fn.directory}"; }
                  else "${home}/${fn}";

  nixosHome = "/home/binarin";
  darwinHome = "/Users/binarin";

in {
  flake.nixosModules.user-binarin = { config, lib, ... }: {
    key = "nixos-config.users.binarin";

    imports = [
      self.nixosModules.impermanence-new
      self.nixosModules.home-manager
    ];

    users.users = {
      binarin = {
        description = "Alexey Lebedeff";
        uid = 1000;
        isNormalUser = true;
        group = "binarin";
        home = nixosHome;
        shell = "/run/current-system/sw/bin/zsh";
        extraGroups = [
          "dialout"
          "docker"
          "i2c"
          "libvirtd"
          "lxd"
          "networkmanager"
          "transmission"
          "users"
          "vboxusers"
          "video"
          "wheel"
          "wireshark"
        ];
        openssh = {
          authorizedKeys.keys = config.lib.publicKeys.ssh.secureForUser "binarin";
          authorizedPrincipals = ["root" "binarin" ];
        };
      };
    };

    users.groups = {
      binarin = {
        gid = 1000;
      };
    };

    nix.settings.trusted-users = [ "binarin" ];

    programs.zsh.enable = true;

    home-manager.users.binarin = { ... }: {
      imports = [
        self.homeModules.user-binarin
        self.homeModules.impermanence
      ];
      config = {
        inherit (config) hostConfig inventoryHostName;
        home.homeDirectory = "/home/binarin";
        home.username = "binarin";
        home.stateVersion = config.system.stateVersion;
      };
    };

    environment.persistence."/persist" = lib.mkIf config.impermanence.enable {
      users.binarin = {
        directories = [
          "personal-workspace"
          "org"
          "annex"
          "finance"
          ".ssh"
        ] ++ config.home-manager.users.binarin.impermanence.persist-directories;
        files = [
        ] ++ config.home-manager.users.binarin.impermanence.persist-files;
      };
    };

    environment.persistence."/local" = lib.mkIf config.impermanence.enable {
      users.binarin = {
        files = [
        ] ++ config.home-manager.users.binarin.impermanence.local-files;
        directories = [
          ".cache/nixos-config"
          ".cache/nix"
          ".local/state/home-manager"
          ".local/state/nix"
        ] ++ config.home-manager.users.binarin.impermanence.local-directories;
      };
    };
  };

  flake.homeModules.user-binarin = {config, osConfig, lib, pkgs, ...}: {
    key = "nixos-config.users.binarin";

    imports = [
      self.homeModules.claude-code
      self.homeModules.direnv
    ];

    programs.git = {
      userName = "Alexey Lebedeff";
      userEmail = "binarin@binarin.info";
    };
  };
}
