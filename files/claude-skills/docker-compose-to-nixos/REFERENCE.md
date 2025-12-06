# Docker Compose to NixOS Technical Reference

Detailed technical reference for converting Docker Compose configurations to NixOS modules with Arion.

## Table of Contents

- [Docker Compose to Arion Mapping](#docker-compose-to-arion-mapping)
- [flakeConfig Pattern Deep Dive](#flakeconfig-pattern-deep-dive)
- [Sops Integration Details](#sops-integration-details)
- [Advanced Patterns](#advanced-patterns)
- [Troubleshooting Guide](#troubleshooting-guide)

---

## Docker Compose to Arion Mapping

### Basic Structure

**Docker Compose:**
```yaml
version: "3"
services:
  app:
    # service configuration
volumes:
  data:
networks:
  custom:
```

**Arion:**
```nix
virtualisation.arion.projects.project-name = {
  serviceName = "project-name-docker-compose";
  settings = {
    services = {
      app = {
        service = {
          # service configuration
        };
      };
    };
  };
};
```

**Key differences:**
- No `version` field in Arion
- Services go under `settings.services.{name}.service`
- Volumes are not declared, just used in mounts
- Networks are implicit (all services in same project can communicate)

### Service Attributes

| Docker Compose | Arion/Nix | Example | Notes |
|----------------|-----------|---------|-------|
| `image: "foo:1.0"` | `image = "foo:1.0";` | `image = "postgres:16.1";` | Always pin version |
| `container_name: "foo"` | `container_name = "foo";` | `container_name = "myapp";` | Optional |
| `restart: unless-stopped` | `restart = "unless-stopped";` | String value | |
| `ports: ["80:80"]` | `ports = [ "80:80" ];` | Array of strings | Public ports |
| `expose: ["5432"]` | `expose = [ "5432" ];` | Array of strings | Internal only |
| `volumes: ["./data:/data"]` | `volumes = ["/host:/container"];` | Absolute paths | |
| `environment:` | `environment = { };` | Attribute set | Non-sensitive |
| `env_file: [".env"]` | `env_file = [path];` | Array of paths | For secrets |
| `command: ["cmd", "arg"]` | `command = ["cmd" "arg"];` | Array of strings | |
| `depends_on: ["db"]` | `depends_on = ["db"];` | Array of strings | Startup order |
| `networks: ["custom"]` | N/A | Implicit | All in same network |
| `labels:` | `labels = { };` | Attribute set | Docker labels |
| `user: "1000:1000"` | `user = "1000:1000";` | String | Not recommended, use PUID/PGID |

### Volume Mapping

**Docker Compose named volumes:**
```yaml
volumes:
  data:
  database:
```

**Arion (no declaration needed):**
```nix
# Just use absolute paths in service volumes
volumes = [
  "/mnt/service/data:/data"
  "/mnt/service/var/database:/var/lib/postgresql/data"
];
```

**Pattern:**
- Host path: `/mnt/service-name/{data,var/component}/`
- Container path: Application-specific (varies by image)

### Network Configuration

**Docker Compose:**
```yaml
networks:
  frontend:
  backend:

services:
  app:
    networks:
      - frontend
      - backend
```

**Arion:**
```nix
# All services in same project automatically share a network
# No explicit configuration needed
# Services can communicate by container name or service name
```

**Service discovery:**
- Use container name: `http://postgres:5432`
- Use service name if no container_name: `http://database:5432`

### Environment Variables

**Docker Compose:**
```yaml
environment:
  - KEY=value
  - SECRET=${SECRET}
env_file:
  - .env
```

**Arion:**
```nix
# Static values
environment = {
  KEY = "value";
  ANOTHER_KEY = "another-value";
};

# Secrets via sops template
env_file = [
  config.sops.templates.service-env.path
];
```

**Best practices:**
- **Static config**: Use `environment` block
- **Secrets**: Use `env_file` with sops template
- **UID/GID**: Include in sops template with inventory values

### Dependencies

**Docker Compose:**
```yaml
depends_on:
  - database
  - cache
```

**Arion:**
```nix
depends_on = [
  "database"
  "cache"
];
```

**Behavior:**
- Ensures dependent services start first
- Does NOT wait for "ready" state (use healthchecks for that)
- String names must match service names in same project

---

## flakeConfig Pattern Deep Dive

### The Problem

NixOS modules have two levels of `config` in scope:

1. **Flake-level config**: Available in outer function `{ inputs, self, config, ... }:`
2. **Module-level config**: Available in module function `{ config, lib, ... }:`

Inventory data (like `inventory.usersGroups`) exists at the flake level, not module level.

### The Solution

```nix
{
  inputs,
  self,
  config,    # <-- Flake-level config
  ...
}:
let
  flakeConfig = config;  # Save reference before shadowing
in
{
  flake.nixosModules.service-name =
    { config, lib, ... }:  # <-- Module-level config (shadows outer)
    {
      config = {
        # Access flake-level inventory
        users.users.service = {
          uid = flakeConfig.inventory.usersGroups.systemUsers.service.uid;
        };

        # Access module-level config
        sops.templates."env".content = ''
          SECRET=${config.sops.placeholder."service/secret"}
        '';
      };
    };
}
```

### Why This Matters

**Without flakeConfig (WRONG):**
```nix
{ inputs, self, ... }:  # No config parameter
{
  flake.nixosModules.service-name = { config, lib, ... }: {
    users.users.service = {
      uid = config.inventory.usersGroups.systemUsers.service.uid;
      # ERROR: config.inventory doesn't exist at module level
    };
  };
}
```

**With flakeConfig (CORRECT):**
```nix
{ inputs, self, config, ... }:
let
  flakeConfig = config;
in
{
  flake.nixosModules.service-name = { config, lib, ... }: {
    users.users.service = {
      uid = flakeConfig.inventory.usersGroups.systemUsers.service.uid;
      # SUCCESS: flakeConfig refers to flake-level config
    };
  };
}
```

### Pattern in Codebase

**modules/inventory.nix:**
```nix
{ self, lib, config, ... }:
let
  flakeConfig = config;  # Flake level
  # ...
in
{
  options = {
    inventory.usersGroups = lib.mkOption {
      type = lib.types.raw;
      default = usersGroups;
    };
  };
}
```

**modules/lxc.nix:**
```nix
{ config, ... }:
let
  flakeConfig = config;  # Flake level
in
{
  flake.nixosModules.lxc = { config, lib, ... }: {
    systemd.network.networks."40-lxc" = {
      dns = flakeConfig.inventory.networks.home.dns;
      # Uses flakeConfig for flake-level inventory data
    };
  };
}
```

### Common Mistakes

❌ **Mistake 1: No flakeConfig binding**
```nix
{ inputs, self, ... }:  # Missing config parameter
{
  flake.nixosModules.foo = { config, ... }: {
    users.users.foo.uid = config.inventory.usersGroups.systemUsers.foo.uid;
    # ERROR: inventory not available at module level
  };
}
```

❌ **Mistake 2: Wrong level access**
```nix
{ inputs, self, config, ... }:
{
  flake.nixosModules.foo = { config, ... }: {
    users.users.foo.uid = config.inventory.usersGroups.systemUsers.foo.uid;
    # ERROR: Using module-level config instead of flake-level
  };
}
```

✅ **Correct pattern:**
```nix
{ inputs, self, config, ... }:
let
  flakeConfig = config;
in
{
  flake.nixosModules.foo = { config, ... }: {
    users.users.foo.uid = flakeConfig.inventory.usersGroups.systemUsers.foo.uid;
    # SUCCESS: Using flake-level config via flakeConfig
  };
}
```

---

## Sops Integration Details

### Secret Naming Convention

**Hierarchical with `/` separator:**
```
service-name/nextauth-secret
service-name/database-password
service-name/api-key
service-name/smtp-password
```

**Benefits:**
- Clear ownership (which service)
- Organized in sops YAML structure
- Easy to grep and manage
- Follows Kubernetes secret naming patterns

### Sops Template Pattern

**Declaration:**
```nix
sops.secrets."service/secret1" = { };
sops.secrets."service/secret2" = { };
sops.secrets."service/api-key" = { };

sops.templates."service-env".content = ''
  SECRET1=${config.sops.placeholder."service/secret1"}
  SECRET2=${config.sops.placeholder."service/secret2"}
  API_KEY=${config.sops.placeholder."service/api-key"}

  # Static config
  LOG_LEVEL=info

  # Inventory-based
  PUID=${toString flakeConfig.inventory.usersGroups.systemUsers.service.uid}
  PGID=${toString flakeConfig.inventory.usersGroups.systemUsers.service.gid}
'';
```

**Usage:**
```nix
env_file = [
  config.sops.templates.service-env.path
];
```

**Important:**
- Template content is bash-like, not Nix
- Use `${placeholder}` for secrets
- Use `${toString ...}` for numbers
- Template is written to `/run/secrets-rendered/` at runtime
- File is readable by services via sops permissions

### Secret Lifecycle

1. **Development**: Add placeholder in module
   ```nix
   sops.secrets."service/password" = { };
   ```

2. **Stage module**: `git add modules/services/service.nix`
   - Flakes require files to be staged or committed

3. **Validate**: `just eval-nixos`
   - Ensures sops config is valid (doesn't check actual secrets)

4. **Populate secrets**: After validation passes
   ```bash
   sops set secrets/docker-on-nixos/secrets.yaml '["service"]["password"]' "$(echo '"'$(apg -x16 -m16 -MLCN -n1)'"')"
   ```
   **CRITICAL**: Use nested path `'["service"]["password"]'` NOT flat path `'["service/password"]'`

5. **Deploy**: Secrets are decrypted at runtime on target machine

### Sops File Structure

**CRITICAL: Nested YAML Structure Required**

Secrets referenced in Nix as `"service/secret"` MUST be nested YAML in sops file:

**secrets/docker-on-nixos/secrets.yaml** (encrypted):
```yaml
# CORRECT - nested structure (matches Nix reference "karakeep/nextauth-secret")
karakeep:
  nextauth-secret: ENC[AES256_GCM,data:...,iv:...,tag:...]
  meilisearch-master-key: ENC[AES256_GCM,data:...,iv:...,tag:...]
  openai-api-key: ""

# WRONG - flat structure will cause build errors
karakeep/nextauth-secret: ENC[...]
karakeep/meilisearch-master-key: ENC[...]

# Top-level secrets (no hierarchy) are OK
archivebox-admin-username: ENC[AES256_GCM,data:...,iv:...,tag:...]
archivebox-admin-password: ENC[AES256_GCM,data:...,iv:...,tag:...]
```

**Path mapping:**
- Nix reference: `sops.secrets."karakeep/nextauth-secret"`
- YAML structure: `karakeep:\n  nextauth-secret: value`
- Sops set command: `sops set file.yaml '["karakeep"]["nextauth-secret"]' "value"`

### Advanced Sops Features

**Secret with specific owner/permissions:**
```nix
sops.secrets."service/secret" = {
  owner = "service-user";
  group = "service-group";
  mode = "0440";
};
```

**Secret triggering service restart:**
```nix
sops.secrets."service/config" = {
  restartUnits = [ "service.service" ];
};
```

**Template with owner:**
```nix
sops.templates."service-config" = {
  content = ''
    SECRET=${config.sops.placeholder."service/secret"}
  '';
  owner = "service-user";
  restartUnits = [ "service.service" ];
};
```

---

## Advanced Patterns

### Health Checks

**Docker Compose:**
```yaml
healthcheck:
  test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
  interval: 30s
  timeout: 10s
  retries: 3
  start_period: 40s
```

**Arion:**
```nix
service = {
  image = "myapp:1.0";
  healthcheck = {
    test = ["CMD" "curl" "-f" "http://localhost:8080/health"];
    interval = "30s";
    timeout = "10s";
    retries = 3;
    start_period = "40s";
  };
};
```

### Resource Limits

**Docker Compose:**
```yaml
deploy:
  resources:
    limits:
      cpus: '0.5'
      memory: 512M
    reservations:
      cpus: '0.25'
      memory: 256M
```

**Arion:**
```nix
service = {
  image = "myapp:1.0";
  deploy = {
    resources = {
      limits = {
        cpus = "0.5";
        memory = "512M";
      };
      reservations = {
        cpus = "0.25";
        memory = "256M";
      };
    };
  };
};
```

### Multiple env_files

**Docker Compose:**
```yaml
env_file:
  - .env.common
  - .env.secrets
```

**Arion:**
```nix
env_file = [
  config.sops.templates.common-env.path
  config.sops.templates.secrets-env.path
];
```

### Conditional Service Inclusion

**Pattern for optional services:**
```nix
virtualisation.arion.projects.service = {
  settings = {
    services = lib.optionalAttrs config.services.enable-cache {
      cache = {
        service = {
          image = "redis:7.2";
        };
      };
    } // {
      # Always included services
      app = {
        service = {
          image = "myapp:1.0";
        };
      };
    };
  };
};
```

### Service-Specific UIDs

**For services that need specific UIDs inside container:**
```nix
sops.templates."service-env".content = ''
  # Pass host UIDs to container
  PUID=${toString flakeConfig.inventory.usersGroups.systemUsers.service.uid}
  PGID=${toString flakeConfig.inventory.usersGroups.systemUsers.service.gid}
'';
```

Many Docker images (Jellyfin, qBittorrent, etc.) respect PUID/PGID to run as specific user inside container, ensuring file permissions match host.

---

## Troubleshooting Guide

### Evaluation Errors

**Error: "attribute 'inventory' missing"**
```
Possible causes:
1. Missing inventory module import
2. flakeConfig not defined
3. Using config instead of flakeConfig for inventory

Solution:
- Add to imports: self.nixosModules.inventory
- Ensure: let flakeConfig = config; in
- Use: flakeConfig.inventory.usersGroups
```

**Error: "infinite recursion"**
```
Possible causes:
1. Circular dependency between modules
2. Self-referential sops template

Solution:
- Check module import chain
- Ensure templates don't reference themselves
- Use separate templates if needed
```

**Error: "file does not exist" during eval**
```
Possible causes:
1. New file not staged with git
2. File path is relative instead of absolute

Solution:
- git add new-file.nix
- Use absolute paths for volumes
```

### Runtime Errors

**Container can't write to volume**
```
Symptoms:
- Permission denied errors
- Failed to create directory

Diagnosis:
1. Check host directory ownership:
   ls -la /mnt/service-name/data

2. Should be owned by service user:
   drwxr-xr-x 2 service service 4096 ...

Solution:
- Verify tmpfiles.rules create dirs with correct owner
- Verify PUID/PGID in env match inventory UID/GID
- Check systemd-tmpfiles status: systemctl status systemd-tmpfiles-setup
```

**Secrets not decrypted**
```
Symptoms:
- Empty environment variables
- Service fails with auth errors

Diagnosis:
1. Check sops secret path:
   sops -d secrets/docker-on-nixos/secrets.yaml | grep service-name

2. Check template rendering:
   ls -la /run/secrets-rendered/

Solution:
- Verify secret exists in sops file
- Check machine has sops decryption key
- Verify secret name matches exactly (case-sensitive)
```

**Service can't communicate with dependency**
```
Symptoms:
- Connection refused
- Unknown host

Diagnosis:
1. Check both services are in same project
2. Check depends_on is set
3. Check service/container names

Solution:
- Ensure services are in same virtualisation.arion.projects.name
- Use correct service name for DNS (container_name or service key)
- Add depends_on if not present
```

### Validation Failures

**just eval-nixos fails with "module collision"**
```
Error: The option `services.foo` is defined multiple times

Cause:
- Missing or duplicate key attribute
- Module imported twice

Solution:
- Ensure key = "nixos-config.modules.nixos.unique-name";
- Check imports list for duplicates
- Run: just lint (checks module keys)
```

**just eval-all passes but deploy fails**
```
Possible causes:
1. Secrets not populated on target machine
2. Directory paths don't exist on target
3. Docker not running on target

Solution:
- Check sops can decrypt: sops -d secrets/file.yaml
- Verify tmpfiles.rules will create dirs
- Check docker.service status on target
```

### Git Workflow Issues

**"flake is dirty" warning**
```
Normal during development. Means working tree has uncommitted changes.

To resolve:
- git add changed-files
- git commit -m "message"

Or continue working (warning is harmless during dev)
```

**Evaluation fails after git commit**
```
Symptoms:
- Worked before commit
- Fails after commit

Cause:
- Flakes evaluate from committed state
- Staged file not committed

Solution:
- Ensure all new files are committed
- Check git status for "Changes to be committed"
```

### Common Pitfalls

| Issue | Symptom | Solution |
|-------|---------|----------|
| Using `:latest` | Updates break deployment | Pin to specific version |
| Hardcoded UID/GID | Conflicts with other services | Use inventory allocation |
| Module-level config for inventory | Attribute missing error | Use flakeConfig |
| Relative volume paths | Can't find directory | Use absolute paths |
| Missing depends_on | Race conditions on startup | Add dependency chain |
| Secret in environment block | Plain text exposure | Use sops template |
| Missing key attribute | Module collision | Add unique key |
| Not staging new files | File not found | git add before eval |
