# Docker Compose to NixOS Examples

This document shows complete conversions of real docker-compose configurations to NixOS modules.

## Table of Contents

- [Example 1: Karakeep (Multi-Service App)](#example-1-karakeep-multi-service-app)
- [Example 2: ArchiveBox (With Scheduler)](#example-2-archivebox-with-scheduler)
- [Comparison Table](#comparison-table)

---

## Example 1: Karakeep (Multi-Service App)

Karakeep demonstrates a typical multi-service application with a main app, search backend (Meilisearch), and headless browser (Chrome).

### Original docker-compose.yml

```yaml
version: "3"

services:
  karakeep:
    image: ghcr.io/karakeep-app/karakeep:${KARAKEEP_VERSION:-release}
    container_name: karakeep
    restart: unless-stopped
    ports:
      - "3000:3000"
    volumes:
      - data:/data
    env_file:
      - .env
    depends_on:
      - meilisearch
      - chrome

  meilisearch:
    image: getmeili/meilisearch:v1.13.3
    container_name: meilisearch
    restart: unless-stopped
    volumes:
      - meilisearch:/meili_data
    environment:
      - MEILI_MASTER_KEY=${MEILISEARCH_MASTER_KEY}
      - MEILI_NO_ANALYTICS=true

  chrome:
    image: gcr.io/zenika-hub/alpine-chrome:124
    container_name: chrome
    restart: unless-stopped
    command:
      - --no-sandbox
      - --disable-gpu
      - --disable-dev-shm-usage
      - --remote-debugging-address=0.0.0.0
      - --remote-debugging-port=9222

volumes:
  data:
  meilisearch:
```

### Environment Variables (.env)

```bash
KARAKEEP_VERSION=0.29.1
NEXTAUTH_SECRET=super_random_string
NEXTAUTH_URL=http://localhost:3000
MEILISEARCH_MASTER_KEY=another_random_string
MEILI_ADDR=http://meilisearch:7700
BROWSER_WEB_URL=http://chrome:9222
DATA_DIR=/data
OPENAI_API_KEY=optional_key
```

### Converted NixOS Module

**File: `modules/services/karakeep.nix`**

```nix
{
  inputs,
  self,
  config,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.nixosModules.karakeep =
    { config, lib, ... }:
    let
      inherit (lib) mkDefault;
    in
    {
      key = "nixos-config.modules.nixos.karakeep";

      # Explicit imports for all dependencies this module needs
      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.arion.nixosModules.arion
        self.nixosModules.tailscale
        self.nixosModules.inventory
      ];

      config = {
        # Create karakeep user with fixed UID/GID from inventory for Docker volume permissions
        users.groups.karakeep = {
          gid = flakeConfig.inventory.usersGroups.systemUsers.karakeep.gid;
        };

        users.users.karakeep = {
          isSystemUser = true;
          group = "karakeep";
          uid = flakeConfig.inventory.usersGroups.systemUsers.karakeep.uid;
        };

        # Sops secrets configuration
        sops.secrets."karakeep/nextauth-secret" = { };
        sops.secrets."karakeep/meilisearch-master-key" = { };
        sops.secrets."karakeep/openai-api-key" = { };

        # Sops template for environment variables
        sops.templates."karakeep-env".content = ''
          NEXTAUTH_SECRET=${config.sops.placeholder."karakeep/nextauth-secret"}
          NEXTAUTH_URL=http://localhost:3000
          MEILISEARCH_MASTER_KEY=${config.sops.placeholder."karakeep/meilisearch-master-key"}
          MEILI_ADDR=http://meilisearch:7700
          BROWSER_WEB_URL=http://chrome:9222
          DATA_DIR=/data
          OPENAI_API_KEY=${config.sops.placeholder."karakeep/openai-api-key"}
          PUID=${toString flakeConfig.inventory.usersGroups.systemUsers.karakeep.uid}
          PGID=${toString flakeConfig.inventory.usersGroups.systemUsers.karakeep.gid}
        '';

        # Systemd tmpfiles rules for directory creation
        # Mount structure:
        # - /mnt/karakeep/data: application data
        # - /mnt/karakeep/var/meilisearch: search index data
        systemd.tmpfiles.rules = [
          "d /mnt/karakeep 0755 karakeep karakeep -"
          "d /mnt/karakeep/data 0755 karakeep karakeep -"
          "d /mnt/karakeep/var 0755 karakeep karakeep -"
          "d /mnt/karakeep/var/meilisearch 0755 karakeep karakeep -"
        ];

        # Arion docker-compose configuration
        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.karakeep = {
          serviceName = "karakeep-docker-compose";
          settings = {
            services = {
              # Main Karakeep service
              karakeep = {
                service = {
                  image = "ghcr.io/karakeep-app/karakeep:0.29.1";
                  container_name = "karakeep";
                  restart = "unless-stopped";
                  ports = [ "3000:3000" ];
                  volumes = [
                    "/mnt/karakeep/data:/data"
                  ];
                  env_file = [
                    config.sops.templates.karakeep-env.path
                  ];
                  depends_on = [
                    "meilisearch"
                    "chrome"
                  ];
                };
              };

              # Meilisearch search backend
              meilisearch = {
                service = {
                  image = "getmeili/meilisearch:v1.13.3";
                  container_name = "karakeep-meilisearch";
                  restart = "unless-stopped";
                  expose = [ "7700" ];
                  volumes = [
                    "/mnt/karakeep/var/meilisearch:/meili_data"
                  ];
                  environment = {
                    MEILI_MASTER_KEY = config.sops.placeholder."karakeep/meilisearch-master-key";
                    MEILI_NO_ANALYTICS = "true";
                  };
                };
              };

              # Chrome headless browser for page capture
              chrome = {
                service = {
                  image = "gcr.io/zenika-hub/alpine-chrome:124";
                  container_name = "karakeep-chrome";
                  restart = "unless-stopped";
                  expose = [ "9222" ];
                  command = [
                    "--no-sandbox"
                    "--disable-gpu"
                    "--disable-dev-shm-usage"
                    "--remote-debugging-address=0.0.0.0"
                    "--remote-debugging-port=9222"
                  ];
                };
              };
            };
          };
        };

        # Expose Karakeep as a tailscale service
        services.tailscale.serve = {
          enable = mkDefault true;
          services.karakeep = {
            serviceName = "karakeep";
            protocol = "https";
            target = "localhost:3000";
          };
        };
      };
    };
}
```

### inventory/users-groups.nix Entry

```nix
{
  systemUsers = {
    archivebox = { uid = 2001; gid = 2001; };
    karakeep = { uid = 2002; gid = 2002; };
  };
}
```

### Secrets Population

```bash
# Add secrets to sops file
sops set secrets/docker-on-nixos/secrets.yaml '["karakeep/nextauth-secret"]' "$(echo '"'$(apg -x16 -m16 -MLCN -n1)'"')"
sops set secrets/docker-on-nixos/secrets.yaml '["karakeep/meilisearch-master-key"]' "$(echo '"'$(apg -x16 -m16 -MLCN -n1)'"')"
sops set secrets/docker-on-nixos/secrets.yaml '["karakeep/openai-api-key"]' '""'  # Empty, populate manually
```

### Key Conversion Points

1. **Version pinning**: Changed `${KARAKEEP_VERSION:-release}` to `0.29.1`
2. **Named volumes**: Converted to host paths under `/mnt/karakeep/`
3. **Environment file**: Moved from `.env` to sops template
4. **Secrets**: Extracted to sops with hierarchical naming
5. **User/group**: Added system user with inventory-managed UID/GID
6. **Port exposure**: Main service uses `ports`, internal services use `expose`
7. **Tailscale**: Added for secure external access

---

## Example 2: ArchiveBox (With Scheduler)

ArchiveBox demonstrates a service with multiple containers using the same image (main app + scheduler) and a separate search backend (Sonic).

### Original docker-compose.yml (Reconstructed)

```yaml
version: "3"

services:
  archivebox:
    image: archivebox/archivebox:latest
    container_name: archivebox
    restart: unless-stopped
    ports:
      - "8000:8000"
    volumes:
      - ./data/archivebox:/data
      - ./data/archive:/data/archive
    env_file:
      - .env
    depends_on:
      - sonic

  archivebox_scheduler:
    image: archivebox/archivebox:latest
    container_name: archivebox_scheduler
    restart: unless-stopped
    command:
      - schedule
      - --foreground
      - --update
      - --every=day
    volumes:
      - ./data/archivebox:/data
      - ./data/archive:/data/archive
    environment:
      - TIMEOUT=120
    env_file:
      - .env
    depends_on:
      - sonic

  sonic:
    image: archivebox/sonic:latest
    container_name: sonic
    restart: unless-stopped
    volumes:
      - ./data/sonic:/var/lib/sonic/store
    environment:
      - SEARCH_BACKEND_PASSWORD=${SEARCH_BACKEND_PASSWORD}

volumes:
  data:
  archive:
  sonic:
```

### Environment Variables (.env)

```bash
ADMIN_USERNAME=admin
ADMIN_PASSWORD=secret_password
SEARCH_BACKEND_PASSWORD=sonic_password
ALLOWED_HOSTS=*
PUBLIC_INDEX=True
PUBLIC_SNAPSHOTS=True
PUBLIC_ADD_VIEW=False
SEARCH_BACKEND_ENGINE=sonic
SEARCH_BACKEND_HOST_NAME=sonic
PUID=2001
PGID=2001
```

### Converted NixOS Module

**File: `modules/services/archivebox.nix`**

```nix
{
  inputs,
  self,
  config,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.nixosModules.archivebox =
    { config, lib, ... }:
    let
      inherit (lib) mkDefault;
    in
    {
      key = "nixos-config.modules.nixos.archivebox";

      # Explicit imports for all dependencies this module needs
      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.arion.nixosModules.arion
        self.nixosModules.tailscale
        self.nixosModules.inventory
      ];

      config = {
        # Create archivebox user with fixed UID/GID from inventory for Docker volume permissions
        users.groups.archivebox = {
          gid = flakeConfig.inventory.usersGroups.systemUsers.archivebox.gid;
        };

        users.users.archivebox = {
          isSystemUser = true;
          group = "archivebox";
          uid = flakeConfig.inventory.usersGroups.systemUsers.archivebox.uid;
        };

        # Sops secrets configuration
        sops.secrets."archivebox-admin-username" = { };
        sops.secrets."archivebox-admin-password" = { };
        sops.secrets."archivebox-search-password" = { };

        # Sops template for environment variables
        sops.templates."archivebox-env".content = ''
          ADMIN_USERNAME=${config.sops.placeholder."archivebox-admin-username"}
          ADMIN_PASSWORD=${config.sops.placeholder."archivebox-admin-password"}
          SEARCH_BACKEND_PASSWORD=${config.sops.placeholder."archivebox-search-password"}
          ALLOWED_HOSTS=*
          PUBLIC_INDEX=True
          PUBLIC_SNAPSHOTS=True
          PUBLIC_ADD_VIEW=False
          SEARCH_BACKEND_ENGINE=sonic
          SEARCH_BACKEND_HOST_NAME=sonic
          PUID=${toString flakeConfig.inventory.usersGroups.systemUsers.archivebox.uid}
          PGID=${toString flakeConfig.inventory.usersGroups.systemUsers.archivebox.gid}
        '';

        # Systemd tmpfiles rules for directory creation
        # Mount structure: Only 2 host mounts
        # - /mnt/archivebox/data: archived data
        # - /mnt/archivebox/var: everything else (with subfolders per container)
        systemd.tmpfiles.rules = [
          "d /mnt/archivebox 0755 archivebox archivebox -"
          "d /mnt/archivebox/var 0755 archivebox archivebox -"
          "d /mnt/archivebox/var/archivebox 0755 archivebox archivebox -"
          "d /mnt/archivebox/var/sonic 0755 archivebox archivebox -"
          "d /mnt/archivebox/data 0755 archivebox archivebox -"
        ];

        # Arion docker-compose configuration
        virtualisation.arion.backend = "docker";
        virtualisation.arion.projects.archivebox = {
          serviceName = "archivebox-docker-compose";
          settings = {
            services = {
              # Main ArchiveBox service
              archivebox = {
                service = {
                  image = "archivebox/archivebox:latest";
                  container_name = "archivebox";
                  restart = "unless-stopped";
                  ports = [ "8000:8000" ];
                  volumes = [
                    "/mnt/archivebox/var/archivebox:/data"
                    "/mnt/archivebox/data:/data/archive"
                  ];
                  env_file = [
                    config.sops.templates.archivebox-env.path
                  ];
                  depends_on = [ "sonic" ];
                };
              };

              # Scheduler service for periodic archiving
              archivebox_scheduler = {
                service = {
                  image = "archivebox/archivebox:latest";
                  container_name = "archivebox_scheduler";
                  restart = "unless-stopped";
                  command = [
                    "schedule"
                    "--foreground"
                    "--update"
                    "--every=day"
                  ];
                  volumes = [
                    "/mnt/archivebox/var/archivebox:/data"
                    "/mnt/archivebox/data:/data/archive"
                  ];
                  environment = {
                    TIMEOUT = "120";
                  };
                  env_file = [
                    config.sops.templates.archivebox-env.path
                  ];
                  depends_on = [ "sonic" ];
                };
              };

              # Sonic search backend
              sonic = {
                service = {
                  image = "archivebox/sonic:latest";
                  container_name = "sonic";
                  restart = "unless-stopped";
                  expose = [ "1491" ];
                  volumes = [
                    "/mnt/archivebox/var/sonic:/var/lib/sonic/store"
                  ];
                  environment = {
                    SEARCH_BACKEND_PASSWORD = config.sops.placeholder."archivebox-search-password";
                  };
                };
              };
            };
          };
        };

        # Expose ArchiveBox as a tailscale service
        services.tailscale.serve = {
          enable = mkDefault true;
          services.archivebox = {
            serviceName = "archivebox";
            protocol = "https";
            target = "localhost:8000";
          };
        };
      };
    };
}
```

### Key Conversion Points

1. **Image versions**: Note that `:latest` is still used (should be pinned in production)
2. **Multiple containers, same image**: Both archivebox and archivebox_scheduler use same image
3. **Command override**: Scheduler uses custom command
4. **Shared volumes**: Both main and scheduler mount same volumes
5. **Environment mixing**: Scheduler has its own environment vars plus shared env_file
6. **Secret in environment**: Sonic's password uses sops placeholder directly
7. **Directory structure**: Organized with `/var/` subdirectories for each component

---

## Comparison Table

| Aspect | Docker Compose | NixOS Module |
|--------|---------------|--------------|
| **Secrets** | `.env` file | sops-nix with templates |
| **Volumes** | Named volumes or relative paths | Absolute paths under `/mnt/` |
| **User/Group** | PUID/PGID env vars | System user with inventory UID/GID |
| **Dependencies** | implicit (docker-compose.yaml) | Explicit imports |
| **Network** | Exposed ports | Tailscale serve integration |
| **Validation** | `docker-compose config` | `just eval-nixos` |
| **Version pinning** | Optional | Required (no `:latest`) |
| **State** | Mutable (files on disk) | Declarative (module config) |
| **Secrets storage** | `.env` file (plaintext) | Encrypted with sops |

## Pattern Observations

### Service Naming
- **Docker Compose**: Service names in YAML
- **NixOS**: Attribute names in `services = { name = { ... }; }`

### Port Handling
- **Public ports**: Use `ports = [ "8000:8000" ]`
- **Internal ports**: Use `expose = [ "7700" ]`
- **No exposure needed**: Omit both (services can communicate via service names)

### Volume Mounts
- Always use absolute paths on host side
- Organize by service under `/mnt/service-name/`
- Use `/mnt/service-name/data` for user data
- Use `/mnt/service-name/var/component` for component-specific state

### Environment Variables
- **Secrets**: Always in sops template
- **Static config**: Can be in template or `environment` block
- **UIDs**: Always from inventory via flakeConfig

### Dependencies
- Explicit dependency chain via `depends_on`
- Ensures correct startup order
- Critical for services requiring databases, caches, etc.
