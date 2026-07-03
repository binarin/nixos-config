{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.bentos =
    { lib, pkgs, nixosModulesPath, ... }:
    {
      key = "nixos-config.systemModules.bentos";

      disabledModules = [
        "${inputs.system-manager}/nix/modules/upstream/nixpkgs"
      ];

      imports = [
        self.systemModules.sysctl
        self.systemModules.nix-path
        self.systemModules.compat
        self.systemModules.yum-packages

        "${inputs.system-manager}/nix/modules/upstream/nixpkgs/userborn.nix"
        "${inputs.system-manager}/nix/modules/upstream/nixpkgs/users-groups.nix"

        (nixosModulesPath + "/misc/meta.nix")
        (nixosModulesPath + "/misc/ids.nix")
        (nixosModulesPath + "/services/system/userborn.nix")
        (nixosModulesPath + "/system/build.nix")
      ];

      config = {
        system-manager.allowAnyDistro = true;

        services.graphical-desktop.enable = lib.mkDefault false;
        impermanence.enable = lib.mkDefault false;
        services.userborn.enable = false;

        environment.systemPackages = [
          pkgs.system-manager
        ];

        boot.kernel.sysctl = {
          "kernel.yama.ptrace_scope" = lib.mkDefault 0;
        };
      };

      options = {
        system.activationScripts.users = lib.mkOption {
          type = lib.types.str;
          default = "";
        };

        system.userActivationScripts = lib.mkOption {
          type = lib.types.attrsOf lib.types.unspecified;
          default = { };
        };
      };
    };
}
