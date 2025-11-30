{
  self,
  inputs,
  ...
}:
{
  flake-file.inputs = {
    impermanence.url = "github:nix-community/impermanence";
  };

  flake.nixosModules.impermanence =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.impermanence";
      imports = [
        inputs.impermanence.nixosModules.impermanence
        self.nixosModules.sops
        self.nixosModules.home-manager
      ];

      # impermanence is too eager to apply checks, even to `evironment.persistence.XXX` which are not enabled.
      # Let's introduce a level of indirection here so that `evironment.persistence.XXX` is really defined when we
      # enable impermanence, yet persistence rules can be defined throughout the configuration. As a side benefit, no
      # typo will be possible in "/local" and "/persist" names.
      options = {
        impermanence = {
          enable = lib.mkEnableOption "Enable impermanence";
          persist = {
            directories = lib.mkOption {
              description = "Directories to be persisted on /persist mount (to be backed up)";
              type = with lib.types; listOf (either str (lazyAttrsOf raw));
              default = [ ];
            };
            files = lib.mkOption {
              description = "Files to be persisted on /persist mount (to be backed up)";
              type = with lib.types; listOf (either str (lazyAttrsOf raw));
              default = [ ];
            };
          };
          local = {
            directories = lib.mkOption {
              description = "Directories to be persisted on /local mount (not backed up, mostly for big expensive caches)";
              type = with lib.types; listOf (either str (lazyAttrsOf raw));
              default = [ ];
            };
            files = lib.mkOption {
              description = "Files to be persisted on /local mount (not backed up, mostly for big expensive caches)";
              type = with lib.types; listOf (either str (lazyAttrsOf raw));
              default = [ ];
            };
          };
        };
      };

      config = lib.mkIf config.impermanence.enable (
        lib.mkMerge [
          {
            home-manager.sharedModules = [
              self.homeModules.impermanence
            ];

            boot.initrd.systemd.emergencyAccess = true;
            boot.initrd.systemd.initrdBin = with pkgs; [
              rsync
            ];
            boot.supportedFilesystems = [
              "ext4"
              "vfat"
              "exfat"
            ];

            users.mutableUsers = false;

            programs.fuse.userAllowOther = true;

            sops.age.sshKeyPaths = lib.mkForce [ "/persist/ssh/ssh_host_ed25519_key" ];

            system.activationScripts = {
              "manual-impermanence-create-dirs" = {
                deps = [
                  "users"
                  "groups"
                ];
                text = ''
                  mkdir -p /local/var/lib/tailscale/
                '';
              };
            };

            services.openssh = {
              enable = true;
              hostKeys = [
                {
                  path = "/persist/ssh/ssh_host_ed25519_key";
                  type = "ed25519";
                }
                {
                  path = "/persist/ssh/ssh_host_rsa_key";
                  type = "rsa";
                  bits = 4096;
                }
                {
                  path = "/persist/ssh/ssh_host_ecdsa_key";
                  type = "ecdsa";
                }
              ];
            };

            systemd.services.tailscaled.serviceConfig.BindPaths = [
              "/local/var/lib/tailscale:/var/lib/tailscale"
            ];

            environment.persistence."/persist" = {
              enable = true;
              hideMounts = true;
              directories = [
                "/var/lib/nixos"
                "/root/.ssh"
                "/etc/NetworkManager/system-connections"
              ];
              files = [
                "/etc/machine-id"
              ];
              users =
                with lib;
                flip mapAttrs config.home-manager.users (
                  _u: h: {
                    files = h.impermanence.persist-files;
                    directories = h.impermanence.persist-directories;
                  }
                );
            };

            environment.persistence."/local" = {
              enable = true;
              hideMounts = true;
              directories = [
                "/var/lib/systemd/coredump"
                "/var/lib/systemd/timers"
              ]
              ++ lib.optionals config.boot.isContainer [
                "/var/log" # XXX only do it when there is no actual mount yet. But without infinite recursion.
              ];
              users =
                with lib;
                flip mapAttrs config.home-manager.users (
                  _u: h: {
                    files = h.impermanence.local-files;
                    directories = h.impermanence.local-directories;
                  }
                );
            };
          }

          (lib.mkIf
            (
              config.fileSystems ? "/" && config.fileSystems."/".fsType == "zfs" && config.fileSystems."/".enable
            )
            {
              boot.initrd.systemd.enable = true;
              boot.initrd.systemd.services.impermanence-reset = {
                description = "reset root filesystem";

                wantedBy = [ "sysroot.mount" ];

                after = [ "zfs-import-rpool.service" ];
                requires = [ "zfs-import-rpool.service" ];

                path = with pkgs; [ zfs ];
                unitConfig.DefaultDependencies = "no";
                serviceConfig.Type = "oneshot";
                script = ''
                  echo Rolling back "${config.fileSystems."/".device}"
                  zfs rollback -r ${config.fileSystems."/".device}@blank
                '';
              };
            }
          )

          (lib.mkIf
            (
              config.fileSystems ? "/"
              && config.fileSystems."/".fsType == "btrfs"
              && config.fileSystems."/".enable
            )
            {
              boot.initrd.systemd.enable = true;
              boot.initrd.systemd.services.impermanence-root-rollback = {
                description = "Rollback root btrfs subvolume to a pristine state";
                wantedBy = [
                  "initrd.target"
                ];
                after = [
                  "dev-main-all.device"
                ];
                before = [
                  "sysroot.mount"
                ];
                path = with pkgs; [
                  btrfs-progs
                  findutils
                  # core-utils / util-linux "mount" are already in /bin
                ];
                unitConfig.DefaultDependencies = "no";
                serviceConfig.Type = "oneshot";
                script = ''
                  export PATH="$PATH:/bin"
                  mkdir /btrfs_tmp
                  mount /dev/main/all /btrfs_tmp
                  if [[ -e /btrfs_tmp/root ]]; then
                      mkdir -p /btrfs_tmp/old_roots
                      timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
                      mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
                  fi

                  delete_subvolume_recursively() {
                      IFS=$'\n'
                      for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
                          delete_subvolume_recursively "/btrfs_tmp/$i"
                      done
                      btrfs subvolume delete "$1"
                  }

                  for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
                      delete_subvolume_recursively "$i"
                  done

                  btrfs subvolume create /btrfs_tmp/root

                  # They are created with 'Q' by tmpfiles, let's prevent creating those subvolumes
                  mkdir -p /btrfs_tmp/root/var/lib/{machines,portables}

                  umount /btrfs_tmp
                '';
              };
            }
          )

          (lib.mkIf config.services.homebox.enable {
            environment.persistence."/persist" = {
              directories = [
                "/var/lib/homebox"
              ];
            };
          })

          (lib.mkIf config.services.caddy.enable {
            services.caddy.dataDir = "/local/var/lib/caddy";
            systemd.tmpfiles.rules = [
              "d /local/var/lib/caddy 0700 ${config.services.caddy.user} ${config.services.caddy.group} - -"
            ];
          })

          (lib.mkIf config.virtualisation.docker.enable {
            assertions = [
              {
                assertion = config.fileSystems ? "/var/lib/docker";
                message = "Docker's /var/lib/docker should be persisted, either by impermanence or explicitely separately mounted";
              }
            ];
          })

          (lib.mkIf config.virtualisation.libvirtd.enable {
            assertions = [
              {
                assertion = config.fileSystems ? "/var/lib/libvirt";
                message = "/var/lib/libvirt should be persisted, either by impermanence or explicitely separately mounted";
              }
            ];
          })
        ]
      );
    };

  flake.homeModules.impermanence =
    {
      lib,
      config,
      osConfig,
      ...
    }:
    let
      homeDir = config.home.homeDirectory;
      safeDir = "/persist${homeDir}";
      localDir = "/local${homeDir}";
      localCache = "${localDir}/.cache";
      safeState = "${safeDir}/.state";
      garbageDir = "${config.home.homeDirectory}/.garbage";
    in
    {
      key = "nixos-config.modules.home.impermanence";

      imports = [
        inputs.impermanence.homeManagerModules.impermanence
        self.homeModules.sops
      ];

      options.impermanence = {
        persist-files = lib.mkOption {
          type = with lib.types; listOf (either str (lazyAttrsOf raw));
          default = [ ];
        };
        persist-directories = lib.mkOption {
          type = with lib.types; listOf (either str (lazyAttrsOf raw));
          default = [ ];
        };
        local-files = lib.mkOption {
          type = with lib.types; listOf (either str (lazyAttrsOf raw));
          default = [ ];
        };
        local-directories = lib.mkOption {
          type = with lib.types; listOf (either str (lazyAttrsOf raw));
          default = [
            ".config/autostart"
          ];
        };
      };

      config = lib.mkIf osConfig.impermanence.enable (
        lib.mkMerge [
          {
            programs.atuin.settings.db_path = "${safeState}/atuin/history.db";

            home.sessionVariables = {
              IMPERMANENCE_LOCAL_CACHE = "${localCache}";
            };

            programs.zsh.dotDir = "${config.xdg.configHome}/zsh";
            programs.zsh.history.path = "${localCache}/zsh_history";

            xdg = {
              enable = true;
            };

            impermanence.persist-directories = [
              "Desktop"
              "Documents"
              "Downloads"
              "Music"
              "Pictures"
              "Videos"
            ];

            xdg.userDirs = {
              enable = true;
              desktop = "${homeDir}/Desktop";
              documents = "${homeDir}/Documents";
              download = "${homeDir}/Downloads";
              music = "${homeDir}/Music";
              pictures = "${homeDir}/Pictures";
              videos = "${homeDir}/Videos";
            };

            # XXX ~/.config permission issues, https://github.com/nix-community/impermanence/issues/74
            # impermanence.persist-files = [
            #   ".config/sops/age/keys.txt"
            # ];
          }

          (lib.mkIf config.programs.starship.enable {
            home.sessionVariables = {
              STARSHIP_CACHE = "${garbageDir}/starship";
            };
          })

          (lib.mkIf config.programs.zoxide.enable {
            home.sessionVariables = {
              _ZO_DATA_DIR = "${localCache}/zoxide";
            };
          })

          (lib.mkIf osConfig.security.pam.services.login.kwallet.enable {
            impermanence.local-directories = [ ".local/share/kwallet" ];
          })
        ]
      );
    };
}
