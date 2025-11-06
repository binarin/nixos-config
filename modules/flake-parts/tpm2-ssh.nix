{lib, ...}:
{
  flake.nixosModules.tpm2-ssh = { config, pkgs, lib, ...}: let
    userOptions = {
      options.openssh.tpm2.enable = lib.mkEnableOption "Make it possible to use SSH keys in TPM2";
    };
    usersWithTpm = with lib; attrNames (
      flip filterAttrs config.users.users (
        n: u: u.openssh.tpm2.enable
      )
    );
  in {
    key = "nixos-config.tpm2-ssh";

    options = {
      users.users = lib.mkOption {
        type = with lib.types; attrsOf (submodule userOptions);
      };
    };

    config = lib.mkIf (lib.length usersWithTpm > 0) {
      environment.systemPackages = with pkgs; [
        tpm2-tools
      ];

      programs.ssh.agentPKCS11Whitelist = "/run/current-system/sw/lib/*";

      security.tpm2 = {
        enable = true;
        pkcs11.enable = true;
        abrmd.enable = true;
        pkcs11.package = pkgs.tpm2-pkcs11.overrideAttrs (f: p: {
          configureFlags = [ "--disable-fapi" ];
          patches = p.patches ++ [
            (config.lib.self.file "0002-remove-fapi-message.patch")
          ];
        });
        tctiEnvironment.enable = true;  # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
      };

      environment.variables.TSS2_LOG = "fapi+NONE";

      users.users = lib.genAttrs usersWithTpm (_: { extraGroups = [ "tss" ]; });
    };
  };
}
