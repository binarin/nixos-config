{
  config,
  self,
  inputs,
  ...
}:
let
  flakeConfig = config;
in
{
  flake.nixosModules.baseline =
    {
      lib,
      pkgs,
      inventoryHostName,
      specialArgs,
      self',
      ...
    }:
    {
      key = "nixos-config.modules.nixos.baseline";

      imports =
        (lib.optional (
          specialArgs ? inventoryHostName && inventoryHostName != "iso" && inventoryHostName != "octopi"
        ) inputs.nixpkgs.nixosModules.readOnlyPkgs)
        ++ [
          "${inputs.srvos}/nixos/common/update-diff.nix"

          self.nixosModules.ci
          self.nixosModules.emacs
          self.nixosModules.eternal-terminal
          self.nixosModules.git
          self.nixosModules.interactive-cli
          self.nixosModules.inventory
          self.nixosModules.monitored
          self.nixosModules.nix
          self.nixosModules.security
          self.nixosModules.sops
          self.nixosModules.ssh-known-hosts
          self.nixosModules.sshd
          self.nixosModules.tailscale
          self.nixosModules.use-nix-cache

          self.nixosModules.binarin-baseline
        ]
        ++ (lib.optional (specialArgs ? clan-core) self.nixosModules.clan-baseline);

      _module.args.self' = {
        packages = self.packages."${pkgs.stdenv.hostPlatform.system}" // {
          xpu-smi = pkgs.callPackage ../../packages/xpu-smi { };
        };
      };

      _module.args.inputs' =
        with lib;
        pipe self.inputs [
          (lib.filterAttrs (_: v: v ? packages))
          (lib.mapAttrs (
            _: v: {
              packages = v.packages."${pkgs.stdenv.hostPlatform.system}";
            }
          ))
        ];

      services.dbus.implementation = lib.mkDefault "broker";
      environment.enableAllTerminfo = true;

      time.timeZone = lib.mkDefault "Europe/Amsterdam";

      i18n.defaultLocale = "nl_NL.UTF-8";
      i18n.extraLocales = [ "all" ];

      environment.systemPackages = with pkgs; [
        psmisc # pstree
        usbutils
        pciutils
        apg
        net-tools # netstat
        dysk
        self'.packages.nix-store-edit
        bpftrace
      ];

      programs.bat.enable = true;
      programs.direnv.enable = true;
      programs.fzf = {
        fuzzyCompletion = true;
        keybindings = true;
      };
      programs.git.enable = true;
      programs.htop.enable = true;
      programs.iftop.enable = true;
      programs.iotop.enable = true;
      programs.mosh.enable = true;
      programs.ssh.startAgent = true;
      programs.tcpdump.enable = true;
      programs.tmux.enable = true;
      programs.traceroute.enable = true;
      programs.zoxide.enable = true;
      security.sudo = {
        enable = true;
        wheelNeedsPassword = false;
      };
    };
}
