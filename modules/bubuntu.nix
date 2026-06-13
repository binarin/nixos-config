{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.bubuntu =
    { lib, pkgs, nixosModulesPath, ... }:
    {
      key = "nixos-config.systemModules.bubuntu";

      disabledModules = [
        "${inputs.system-manager}/nix/modules/upstream/nixpkgs"
        "${inputs.system-manager}/nix/modules/upstream/nixpkgs/nginx.nix"
        (nixosModulesPath + "/services/web-servers/nginx/")
      ];

      imports =
        [
          self.systemModules.sysctl
          self.systemModules.nix-path
          self.systemModules.logind
          self.systemModules.uwsm
          self.systemModules.apt-packages
          self.systemModules.chrome-policies
          self.modules.generic.chrome-policies

          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/firewall.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/nix.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/programs/ssh.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/security-wrappers.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/security/sudo.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/userborn.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/users-groups.nix"
          "${inputs.system-manager}/nix/modules/upstream/sops-nix.nix"
          "${inputs.system-manager}/nix/modules/upstream/nixpkgs/openssh.nix"
        ]
        ++ map (path: nixosModulesPath + path) [
          "/misc/meta.nix"
          "/misc/ids.nix"
          "/security/acme/"
          "/security/sudo.nix"
          "/security/wrappers/"
          "/config/nix.nix"
          "/services/system/userborn.nix"
          "/system/build.nix"
        ];

      options = {
        lib = lib.mkOption {
          type = lib.types.attrsOf lib.types.raw;
          default = { };
          internal = true;
        };

        system.activationScripts.users = lib.mkOption {
          type = lib.types.str;
          default = "";
        };

        system.userActivationScripts = lib.mkOption {
          type = lib.types.attrsOf lib.types.unspecified;
          default = { };
        };

        fonts.fontconfig.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };

        i18n.glibcLocales = lib.mkOption {
          type = lib.types.package;
          default = pkgs.glibcLocales;
          defaultText = lib.literalExpression "pkgs.glibcLocales";
        };
      };

      config = {
        environment.systemPackages = [
          pkgs.system-manager
        ];

        boot.kernel.sysctl = {
          "kernel.unprivileged_userns_clone" = lib.mkDefault 1;
          "kernel.apparmor_restrict_unprivileged_userns" = lib.mkDefault 0;
        };

        services.logind.lidSwitchExternalPower = lib.mkDefault "ignore";
        services.logind.lidSwitchDocked = lib.mkDefault "ignore";
      };
    };
}
