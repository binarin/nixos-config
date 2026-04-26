Adding new machine
------------------

- Create from minimal template: stage .nix-file, update name
- (hack) Set tailscale-auth/tailscale-auth externally:
  ```
  ncf ts auth-key --no-ephemeral --reusable | clan vars set template tailscale-auth/tailscale-auth
  ```
- generate the rest of the vars:
  ```
  clan vars generate template
  ```
- Allocate IP, also add mac address generated from hostId
  ```
  ncf machine ip-allocate template
  ```
