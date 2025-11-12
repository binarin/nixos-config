{ ... }:
{
  flake.nixosModules.tpm2-ssh =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.tpm2-ssh";

      options = {
        programs.openssh.tpm2.enable = lib.mkEnableOption "Configure system to be able to use SSH keys in TPM2 (user should be in 'tss' group)";
      };

      config = lib.mkIf config.programs.openssh.tpm2.enable {
        environment.systemPackages = with pkgs; [
          tpm2-tools
        ];

        programs.ssh.agentPKCS11Whitelist = "/run/current-system/sw/lib/*";

        security.tpm2 = {
          enable = true;
          pkcs11.enable = true;
          abrmd.enable = true;
          pkcs11.package = pkgs.tpm2-pkcs11.overrideAttrs (
            _f: p: {
              configureFlags = [ "--disable-fapi" ];
              patches = p.patches ++ [
                (config.lib.self.file "0002-remove-fapi-message.patch")
              ];
            }
          );
          tctiEnvironment.enable = true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
        };

        environment.variables.TSS2_LOG = "fapi+NONE";
      };
    };
}
