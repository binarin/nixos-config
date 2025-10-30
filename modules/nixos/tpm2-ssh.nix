{ pkgs, ...}: {
  config = lib.mkIf false {

  environment.systemPackages = with pkgs; [
    tpm2-tools
  ];

  security.tpm2 = {
    enable = true;
    pkcs11.enable = true;
    abrmd.enable = true;
    pkcs11.package = pkgs.tpm2-pkcs11.overrideAttrs (f: p: {
      configureFlags = [ "--disable-fapi" ];
      patches = p.patches ++ [
        ./0002-remove-fapi-message.patch
      ];
    });
    tctiEnvironment.enable = true;  # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
  };

  environment.variables.TSS2_LOG = "fapi+NONE";
  users.users.binarin.extraGroups = [ "tss" ];

  home-manager.users.binarin.programs.ssh.extraConfig = ''
    PKCS11Provider /run/current-system/sw/lib/libtpm2_pkcs11.so
  '';
  }
}
