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

    trezor-root-pve = {
      description = "Trezor ssh key with root@pve identity";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBAAHsiCVekjyoPDPOGPJFJ5z9FQUfSmThnGWHABNfjz0APf/qcdHHy+Yj2bG0ICcDbBrE10MJiQjrnYRlKUGvAE=";
      secure = true;
      tags = [ "default" ];
    };

    "raum.lynx-lizard.ts.net-pkcs11-root" = {
      description = "root SSH key in pkcs11 at raum - needed for ProxmoxVE operations";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBPKyEXbiRblfl23aNjy73yHjFfV/xct885HH4eIzuW3X2VT0BS/jMEFhNJE7eFJE5vO/QIwJ1FKbEonPRGOvNBI=";
      secure = true;
      force_install_on = {
        raum = [ "root" ];
      };
    };

    "bael.lynx-lizard.ts.net-pkcs11-root" = {
      description = "root SSH key in pkcs11 at raum - needed for ProxmoxVE operations";
      public_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBB2KztRbE1TXYv26vLcezAhNRGw1h2XDzwES0eoARXhtvC2q2JLGNEYyej84wUBi8MkLOmdS1qBxp+24w5dxWDo=";
      secure = true;
      force_install_on = {
        bael = [ "root" ];
        raum = [ "root" ];
      };
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
