# b-db-k system-manager Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add system-manager support for the b-db-k (db.k.b) CentOS host, including a reusable bentos base module, a bootstrap script, and deploy integration.

**Architecture:** A `bentos` system module provides the minimal CentOS baseline (nix PATH, sysctl). The machine config (split from murmur.nix) uses it to build a system-manager profile. A bootstrap script handles first-time nix install + activation on the target. After that, `deploy .#b-db-k -s` handles ongoing deployment.

**Tech Stack:** Nix flakes, system-manager, deploy-rs, bash

---

### Task 1: Create the bentos system module

**Files:**
- Create: `modules/bentos.nix`

- [ ] **Step 1: Create `modules/bentos.nix`**

```nix
{
  self,
  inputs,
  ...
}:
{
  flake.systemModules.bentos =
    { lib, pkgs, ... }:
    {
      key = "nixos-config.systemModules.bentos";

      imports = [
        self.systemModules.sysctl
        self.systemModules.nix-path
      ];

      config = {
        environment.systemPackages = [
          pkgs.system-manager
        ];

        boot.kernel.sysctl = {
          "kernel.yama.ptrace_scope" = lib.mkDefault 0;
        };
      };
    };
}
```

- [ ] **Step 2: Verify it evaluates**

Run: `nix eval .#systemModules.bentos --apply 'x: builtins.typeOf x'`
Expected: `"set"` (or similar confirmation it's a valid module)

- [ ] **Step 3: Commit**

```bash
git add modules/bentos.nix
git commit -m "feat: add bentos system module for CentOS/RHEL hosts"
```

---

### Task 2: Create the b-dev-kvm machine file (split from murmur.nix)

**Files:**
- Create: `modules/machines/b-dev-kvm.nix`
- Modify: `modules/machines/murmur.nix`

- [ ] **Step 1: Create `modules/machines/b-dev-kvm.nix`**

This file contains all booking KVM host config. It needs `makeSystemConfig` (same pattern as murmur.nix) and a pkgs set with the system-manager overlay.

```nix
{
  self,
  inputs,
  lib,
  ...
}:
let
  makeSystemConfig = (import "${self}/lib/make-system-config.nix" {
    inherit lib;
    nixos = "${inputs.nixpkgs}/nixos";
    userborn = inputs.system-manager.inputs.userborn;
    system-manager-src = inputs.system-manager;
  }).makeSystemConfig;

  bDevKvmPkgs = self.configured-pkgs.x86_64-linux.nixpkgs.appendOverlays [
    inputs.system-manager.overlays.default
  ];
in
{
  flake.systemConfigs.b-db-k = makeSystemConfig {
    pkgs = bDevKvmPkgs;
    modules = [
      self.systemModules.bentos
      {
        environment.etc."nix/nix.custom.conf".text = ''
          trusted-users = allebedev root 15008352
        '';
      }
    ];
  };

  flake.deploy.nodes.b-adb-k = {
    hostname = "adb.k.b";
    sshUser = "allebedev";
    profiles.user = {
      path = self.lib.deploy-home-manager self.homeConfigurations.b-dev-kvm;
    };
  };

  flake.deploy.nodes.b-db-k = {
    hostname = "db.k.b";
    sshUser = "allebedev";
    profiles.user = {
      path = self.lib.deploy-home-manager self.homeConfigurations.b-dev-kvm;
    };
    profiles.system = {
      user = "root";
      path = self.lib.deploy-system-manager self.systemConfigs.b-db-k;
    };
  };

  flake.homeConfigurations.b-dev-kvm = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;

    modules = [
      self.homeModules.b-dev-kvm-configuration
    ];

    extraSpecialArgs = {
      osConfig.services.graphical-desktop.enable = false;
      osConfig.impermanence.enable = false;
      self'.packages = self.packages.x86_64-linux;
      inputs' = lib.mapAttrs (_: i: {
        packages = i.packages.x86_64-linux;
      }) inputs;
    };
  };

  flake.homeModules.standalone-home-manager-zsh =
    { lib, ... }:
    {
      key = "nixos-config.modules.home.standalone-home-manager-zsh";
      programs.zsh.envExtra = ''
        export BK_OTEL_ENABLED=false

        if [[ -f $HOME/.nix-profile/bin/zsh ]]; then
            export SHELL=$HOME/.nix-profile/bin/zsh
        fi

        # Ensure system-manager's /run/current-system/sw/bin is in PATH
        # (not guaranteed via /etc/environment on non-NixOS when PAM skips it)
        local sw_bin="/run/current-system/sw/bin"
        if [[ -d $sw_bin && ":$PATH:" != *":$sw_bin:"* ]]; then
            export PATH="$sw_bin:$PATH"
        fi

        if [[ -d $HOME/.local/bin && !( $PATH == *$HOME/.local/bin* ) ]]; then
            export PATH="$HOME/.local/bin:$PATH"
        fi

        if [[ -f $HOME/.cargo/env ]] ; then
            . "$HOME/.cargo/env"
        fi

        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
            . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi

      '';
    };

  flake.homeModules.b-dev-kvm-configuration =
    {
      config,
      lib,
      pkgs,
      inputs',
      ...
    }:
    {
      key = "nixos-config.modules.home.b-dev-kvm-configuration";
      imports = [
        self.homeModules.binarin-zsh
        self.homeModules.binarin-baseline
        self.homeModules.standalone-home-manager-zsh
        self.homeModules.interactive-cli
      ];

      programs.ssh.enable = lib.mkForce false;

      services.emacs = {
        enable = true;
        startWithUserSession = true;
        package = lib.mkForce config.programs.emacs.package;
      };

      services.ssh-agent.enable = lib.mkForce false;

      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [
        inputs.emacs-overlay.overlays.default
        self.overlays.my-emacs
        inputs.nix-ai-tools.overlays.shared-nixpkgs
      ];

      xdg.enable = false;
      programs.zsh.dotDir = config.home.homeDirectory;

      programs.zsh.envExtra = ''
        if [[ -n $SSH_AUTH_SOCK && -e $SSH_AUTH_SOCK && $SSH_AUTH_SOCK != *ssh-agent-stable.sock ]]; then
          ln -sf "$SSH_AUTH_SOCK" "$XDG_RUNTIME_DIR/ssh-agent-stable.sock"
        fi

        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent-stable.sock"
        export BK_DISABLE_EVENTS=true

        # all of this only makes sense when I don't manage my configs with home-manager
        local -a slow_scripts=(
           /etc/profile.d/dotfiles.sh
           /etc/profile.d/bk_completion.sh
           /etc/profile.d/nvm.sh
           /etc/profile.d/ssh_warn.sh
           /etc/profile.d/completion.sh
           /etc/profile.d/puppet_warn.sh
         )

         for f in $slow_scripts; do
           if [[ -s $f ]]; then
             sudo tee $slow_scripts < /dev/null > /dev/null
             break
           fi
         done
      '';

      programs.zsh.initContent = lib.mkMerge [
        (lib.mkOrder 500 ''
          if [[ $TERM == "dumb" ]]; then
            unsetopt zle
            PS1='$ '
            return
          fi
        '')
        ''
          if type -p bk > /dev/null ; then
            source <(bk completion zsh)
          fi
        ''
      ];

      home.activation.we-own-the-configs = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
        $DRY_RUN_CMD rm -f ~/.zshrc ~/.zshenv ~/.bash_profile ~/.bashrc ~/.profile
      '';

      home.activation.fix-permissions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p ~/.cache/oh-my-zsh/completions
        $DRY_RUN_CMD chmod 0755 ~/.cache/oh-my-zsh/completions
      '';

      programs.starship.settings.command_timeout = 2000;

      home.stateVersion = "25.11";
      home.username = "allebedev";
      home.homeDirectory = "/home/allebedev";

      home.packages =
        (with pkgs.llm-agents; [
          claude-code
          workmux
        ])
        ++ (with pkgs; [
          asciinema
          delta
          tramp-rpc-server
          (
            (pkgs.writeShellApplication {
              name = "glab";
              runtimeInputs = [ pkgs.glab ];
              text = ''
                export HTTP_PROXY=http://127.0.0.1:18080
                export HTTPS_PROXY=http://127.0.0.1:18080
                if [ -f "$HOME/.mitmproxy/mitmproxy-ca-cert.pem" ]; then
                  if ! cmp -s "$HOME/.mitmproxy/mitmproxy-ca-cert.pem" /etc/pki/tls/certs/mitmproxy-ca-cert.pem 2>/dev/null; then
                    sudo cp "$HOME/.mitmproxy/mitmproxy-ca-cert.pem" /etc/pki/tls/certs/
                  fi
                fi
                exec ${lib.getExe pkgs.glab} "$@"
              '';
            }).overrideAttrs
            (_: {
              name = "cursed-glab";
            })
          )
          git
        ]);
    };
}
```

- [ ] **Step 2: Remove moved code from `modules/machines/murmur.nix`**

Delete these sections from murmur.nix (lines 370–564):
- `flake.deploy.nodes.b-adb-k` (lines 370–376)
- `flake.deploy.nodes.b-db-k` (lines 378–384)
- `flake.homeConfigurations.b-dev-kvm` (lines 386–401)
- `flake.homeModules.standalone-home-manager-zsh` (lines 403–434)
- `flake.homeModules.b-dev-kvm-configuration` (lines 436–562)

Note: `standalone-home-manager-zsh` moves to the new file because murmur no longer needs it — murmur's home config is now integrated via `self.systemModules.home-manager` inside system-manager. The murmur home module (`murmur-home-allebedev`) still imports it, but since it's defined in the new file as `flake.homeModules.standalone-home-manager-zsh`, it's accessible via `self.homeModules.*` from anywhere.

After removal, murmur.nix ends with the `murmur-home-allebedev` module (ending at line 368) followed by `}`.

- [ ] **Step 3: Verify the flake evaluates**

Run: `nix flake check --no-build 2>&1 | head -30`
Expected: No errors (warnings about unfree are OK)

If there are evaluation errors, they likely come from missing imports or the `nixosModulesPath` argument. The `makeSystemConfig` passes `nixosModulesPath` via `specialArgs` (see `lib/make-system-config.nix` line 56).

- [ ] **Step 4: Build the system-manager config**

Run: `nix build .#systemConfigs.b-db-k --dry-run`
Expected: Shows the derivation to be built without errors

- [ ] **Step 5: Commit**

```bash
git add modules/machines/b-dev-kvm.nix
git add modules/machines/murmur.nix
git commit -m "refactor: split b-dev-kvm config from murmur.nix, add b-db-k system-manager"
```

---

### Task 3: Create the bootstrap script

**Files:**
- Create: `scripts/bootstrap-bentos.sh`

- [ ] **Step 1: Create `scripts/bootstrap-bentos.sh`**

```bash
#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 [--remote-build] <hostname>"
  echo ""
  echo "Bootstrap nix + system-manager on a bentos (CentOS/RHEL) host."
  echo ""
  echo "Options:"
  echo "  --remote-build    Build the system-manager closure on the target host"
  echo "                    (default: build locally and copy)"
  exit 1
}

REMOTE_BUILD=false
HOST=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --remote-build) REMOTE_BUILD=true; shift ;;
    -h|--help) usage ;;
    -*) echo "Unknown option: $1"; usage ;;
    *) HOST="$1"; shift ;;
  esac
done

[[ -z "$HOST" ]] && usage

SSH_USER="allebedev"
NIX_BIN="/nix/var/nix/profiles/default/bin/nix"
NIX_DAEMON_SH="/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
SM_PROFILE="/nix/var/nix/profiles/system-manager-profiles/system-manager"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "==> Checking if nix is installed on $HOST..."
if ssh "$SSH_USER@$HOST" "test -x $NIX_BIN"; then
  echo "    nix already installed."
else
  echo "==> Installing nix on $HOST..."
  ssh "$SSH_USER@$HOST" "curl -fsSL https://install.determinate.systems/nix | sudo sh -s -- install --no-confirm --nix-build-user-id-base 666000 --nix-build-group-id 666000 --extra-conf 'trusted-users = allebedev root 15008352'"
  echo "    nix installed."
fi

FLAKE_REF="$REPO_ROOT#systemConfigs.b-db-k"

if [[ "$REMOTE_BUILD" == "true" ]]; then
  echo "==> Building system-manager config on $HOST..."
  # Copy the flake source and build remotely
  STORE_PATH=$(ssh "$SSH_USER@$HOST" "source $NIX_DAEMON_SH && nix build --no-link --print-out-paths '$FLAKE_REF'")
else
  echo "==> Building system-manager config locally..."
  STORE_PATH=$(nix build --no-link --print-out-paths "$FLAKE_REF")

  echo "==> Copying closure to $HOST..."
  nix copy --to "ssh://$SSH_USER@$HOST" "$STORE_PATH"
fi

echo "==> Activating system-manager on $HOST (store path: $STORE_PATH)..."
ssh -t "$SSH_USER@$HOST" "sudo bash -c '
  source $NIX_DAEMON_SH
  mkdir -p $(dirname $SM_PROFILE)
  $NIX_BIN-env --profile $SM_PROFILE --set $STORE_PATH
  $STORE_PATH/bin/activate
'"

echo "==> Done. system-manager activated on $HOST."
echo "    Future deploys: deploy .#b-db-k -s"
```

- [ ] **Step 2: Make it executable**

Run: `chmod +x scripts/bootstrap-bentos.sh`

- [ ] **Step 3: Commit**

```bash
git add scripts/bootstrap-bentos.sh
git commit -m "feat: add bootstrap script for bentos hosts"
```

---

### Task 4: Build and verify locally

- [ ] **Step 1: Build the system-manager config**

Run: `nix build .#systemConfigs.b-db-k`
Expected: Builds successfully, produces `./result` symlink

- [ ] **Step 2: Inspect the output**

Run: `ls ./result/bin/ && cat ./result/etcFiles.json | python3 -m json.tool | grep -A2 "nix.custom.conf"`
Expected: Shows `activate` in bin, and the nix.custom.conf entry in etcFiles

- [ ] **Step 3: Verify sysctl config is present**

Run: `cat ./result/etcFiles.json | python3 -m json.tool | grep -A2 "sysctl"`
Expected: Shows the `60-system-manager.conf` sysctl entry

- [ ] **Step 4: Verify the home-manager config still builds**

Run: `nix build .#homeConfigurations.b-dev-kvm.activationPackage --dry-run`
Expected: No errors

- [ ] **Step 5: Verify murmur still builds**

Run: `nix build .#systemConfigs.murmur --dry-run`
Expected: No errors (murmur not broken by the split)

---

### Task 5: Bootstrap db.k.b

- [ ] **Step 1: Run the bootstrap script**

Run: `./scripts/bootstrap-bentos.sh db.k.b`
Expected: Installs nix, copies closure, activates system-manager

- [ ] **Step 2: Verify system-manager is active on db.k.b**

Run: `ssh db.k.b 'ls /run/current-system/sw/bin/ | head -5; cat /etc/nix/nix.custom.conf; cat /etc/sysctl.d/60-system-manager.conf; cat /etc/environment.d/50-nix.conf'`
Expected:
- sw/bin contains `system-manager` and `nix` etc.
- nix.custom.conf contains `trusted-users = allebedev root 15008352`
- sysctl conf contains `kernel.yama.ptrace_scope=0`
- 50-nix.conf sets PATH entries

- [ ] **Step 3: Verify deploy works**

Run: `deploy .#b-db-k -s -- --impure`
Expected: Deploys both user and system profiles without error

- [ ] **Step 4: Commit any fixups needed**

If any adjustments were needed during bootstrap, commit them:
```bash
git add -u
git commit -m "fix: adjustments from first b-db-k bootstrap"
```

---

### Task 6: Final verification and cleanup

- [ ] **Step 1: Full flake check**

Run: `nix flake check --no-build 2>&1 | tail -20`
Expected: No errors

- [ ] **Step 2: Verify adb.k.b deploy still works**

Run: `deploy .#b-adb-k -s -- --impure`
Expected: Deploys user profile to adb.k.b without error (no system profile, same as before)

- [ ] **Step 3: Remove build result**

Run: `rm -f result`

- [ ] **Step 4: Final commit if needed**

Only if there are uncommitted changes from fixups.
