{
  ssh_keys = {
    pixel-8-pro-bio = {
      description = "Pixel 8 Pro (GrapheneOS) biometrics key in Termius";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBA+FKpMFeDrObDAqF9Tc0gTDw81J0J2uoDccGzcxzHyWC+dBbmToaKAGNFmAUwFX5U8sazTNTKT2EFEwxd8mHIU=";
      secure = true;
      tags = [ "default" ];
    };

    galaxy-tab = {
      description = "Galaxy Tab biometrics key in Termius";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBPMpogmbCYL1eZcImgz+QDH5xW5BoTDUP4/daxglmaBul2tp9SFnMT7f73Z9rVF5XSsavVm4j8mLNV6t3I2xCKI=";
      secure = true;
      tags = [ "default" ];
    };

    valak-non-discoverable-yubikey-fixed = {
      description = "Non-discoverable key on valak for Yubikey-Fixed";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAICLN4EfvtEitGZ/v73XdSvMT7HvpODsUwfpcYLaNvFwvAAAABHNzaDo=";
      secure = true;
      tags = [ "default" ];
    };

    valak-non-discoverable-yubikey-keychain = {
      description = "Non-discoverable key on valak for Yubikey-Keychain";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIMMHRjB4xfdnmV5VrMUgkAqYEBkbrP08ZIVUJczLFw3HAAAABHNzaDo=";
      secure = true;
      tags = [ "default" ];
    };

    surface-non-discoverable-yubikey-fixed = {
      description = "Non-discoverable key on surface pro for Yubikey-Fixed";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIGYZ5Xx9/t4K+fiStJdA5em5TvhxTND9cvO/NTgS00QZAAAABHNzaDo= fixed surface-8-pro 2024-10-26";
      secure = true;
      tags = [ "default" ];
    };

    surface-non-discoverable-yubikey-keychain = {
      description = "Non-discoverable key on surace pro for Yubikey-Keychain";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAILtssU9pK5SFm0ZoBZoCVznkJLLP2zwcRBBMELmS8zepAAAABHNzaDo= yubikey-keychain surface-8-pro 2024-10-26";
      secure = true;
      tags = [ "default" ];
    };

    trezor-root-pve = {
      description = "Trezor ssh key with root@pve identity";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBAAHsiCVekjyoPDPOGPJFJ5z9FQUfSmThnGWHABNfjz0APf/qcdHHy+Yj2bG0ICcDbBrE10MJiQjrnYRlKUGvAE=";
      secure = true;
      tags = [ "default" ];
    };

    valak-nixos-ed25519 = {
      description = "Unencrypted ed25519 key - binarin@valak";
      public_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCVAKqmUdCkJ1gbi2ZA6vLnmf880U/9v5bfxhChapWB";
      secure = false;
    };

    valak-nixos-rsa = {
      description = "Unencrypted RSA key - binarin@valak";
      public_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCf0GHC44SUJM+3e+Xbiz9kRzYGz6MG7PRsQUKdpUuMomLQQVED87R7KxonK8uX2FoJK8nH5tx1Hkp8GRnTbyaicgrJwCKJqhMv/2q6FZUNmvlXbSqQWiMvFPJ/hS4PHI8gEj+czNfOa4rmBxyRPYSMV3ZYfRajDO0sTlxm0qK/uejiGdVueyrydlTyk7iuYPl+noDK7LDGW1FmydfKpFoHgg8Q8hSSsR7rfAJ/VN0JkDJRned5GARlR/xtml24l/YzO79rGyn6hCa2ZAN3Gp6PKdbzXfSI26PSx0JZiNr3poq5BoupFgFnDP36vCfA7d6TrVnbUSBWb2ZG3mS3S0S+eQFraCWrh/D+m7r/gOrhhV6i5WHiE0n7CnjHZ7OJU8+poRKzTgp89K8zZmhoEYHShKn0IrFM5SWONH4tUQDixVoN445fm1BwlBAExQrNgTAoo3rm7sDgtv1V5qkXe8kwlSfJoobIhN5vNZXAuYeetciymJg2EW0or5uA/ztW3OE=";
      secure = false;
    };

    "raum.lynx-lizard.ts.net-pkcs11-root" = {
      description = "root SSH key in pkcs11 at raum - needed for ProxmoxVE operations";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBPKyEXbiRblfl23aNjy73yHjFfV/xct885HH4eIzuW3X2VT0BS/jMEFhNJE7eFJE5vO/QIwJ1FKbEonPRGOvNBI=";
      secure = true;
      force_install_on = [ "raum" ];
    };

    "bael.lynx-lizard.ts.net-pkcs11-root" = {
      description = "root SSH key in pkcs11 at raum - needed for ProxmoxVE operations";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBB2KztRbE1TXYv26vLcezAhNRGw1h2XDzwES0eoARXhtvC2q2JLGNEYyej84wUBi8MkLOmdS1qBxp+24w5dxWDo=";
      secure = true;
      force_install_on = [
        "bael"
        "raum"
      ];
    };

    "yubi-usb-c-5-nfc" = {
      description = "resident SSH key on Yubikey 5 NFC (usb-c)";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIDaRvW2TCKgLVQuHAv5FyNqQuFidJ+5WhIaV9j6nGU3HAAAABHNzaDo= usb-c yubi - residen ssh fido2";
      secure = true;
      tags = [ "default" "user-ca" ];
    };

    "yubi-usb-a-security-key-nfc" = {
      description = "resident SSH key on Yubikey Security Key (usb-a)";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIBecanJ4q2FCAu5iw1Ah+19Ki5k9bncJ2ROXQQn/Bv8rAAAABHNzaDo= usb-a yubikey security key(fido only)  - resident ssh fido2";
      secure = true;
      tags = [ "default" "user-ca" ];
    };

    "yubi-usb-a-5-nfc-fixed" = {
      description = "resident SSH key on Yubikey 5 NFC (usb-a, fixed)";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIEriEHU1OODgpI5oWyeH0zjyUA8N5MsgUiiYIVNXMdQrAAAABHNzaDo= usb-a yubikey 5 nfc - fixed  - resident ssh fido2";
      secure = true;
      tags = [ "default" "user-ca" ];
    };

    "yubi-usb-a-5-nfc-keychain" = {
      description = "resident SSH key on Yubikey 5 NFC (usb-a, keychain)";
      public_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAINC200GwDhLpi27iCDBppww5QPuRtXACjXsrtJ5hnW0LAAAABHNzaDo= usb-a yubikey 5 nfc - keychain  - resident ssh fido2";
      secure = true;
      tags = [ "default" "user-ca" ];
    };
  };
}
