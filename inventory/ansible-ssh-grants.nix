# Direct key grants for Ansible-managed hosts: registry key id -> host ->
# accounts. Keys come from common's ssh-trust registry. Only the Ansible sshd
# role reads this; NixOS hosts grant keys through swanLake.sshTrust.accounts.
{
  pkcs11-raum-root = {
    raum = [ "root" ];
    valak = [ "root" ];
    qdevice-vm = [ "root" ];
  };

  pkcs11-bael-root = {
    bael = [ "root" ];
    raum = [ "root" ];
    valak = [ "root" ];
    qdevice-vm = [ "root" ];
  };

  pkcs11-valak-root = {
    raum = [ "root" ];
    valak = [ "root" ];
    qdevice-vm = [ "root" ];
  };

  brother-scanner = {
    paperless = [ "paperless" ];
    paperless-nixos = [ "paperless" ];
  };

  claude-nixos-config = {
    proxmox-test = [ "root" ];
  };
}
