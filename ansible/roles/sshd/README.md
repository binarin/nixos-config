SSH Daemon Configuration Role
=============================

This role configures SSH daemon for security and manages SSH authorized keys and trusted CA keys.

Requirements
------------

- Ansible 2.9 or higher
- SSH daemon installed on target hosts
- Root or sudo access to modify SSH configuration

Role Variables
--------------

### SSH Configuration Paths
- `sshd_config_path`: Path to main sshd_config file (default: `/etc/ssh/sshd_config`)
- `sshd_config_d_path`: Path to sshd_config.d directory (default: `/etc/ssh/sshd_config.d`)
- `authorized_keys_path`: Path to authorized_keys.d directory (default: `/etc/ssh/authorized_keys.d`)
- `trusted_user_ca_keys_path`: Path to trusted user CA keys file (default: `/etc/ssh/trusted_user_ca_keys`)

### SSH Security Settings
- `sshd_permit_root_login`: Root login setting (default: `"prohibit-password"`)
- `sshd_password_authentication`: Enable password auth (default: `false`)
- `sshd_kbd_interactive_authentication`: Enable keyboard interactive auth (default: `false`)
- `sshd_use_pam`: Enable PAM (default: `false`)

### SSH Users and Keys
- `sshd_users`: List of users to configure authorized keys for (default: `[]`)
- `ssh_keys`: Dictionary of SSH key definitions (should be provided via vars_files)

### Other Settings
- `sshd_include_config_d`: Include config.d directory (default: `true`)

Dependencies
------------

None.

Example Playbook
----------------

```yaml
- hosts: servers
  vars_files:
    - ssh-public-keys.yaml
  roles:
    - sshd
```

License
-------

MIT-0

Author Information
------------------

Maintained as part of nixos-config infrastructure.
