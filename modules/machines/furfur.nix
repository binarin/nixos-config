{
  self,
  inputs,
  lib,
  config,
  ...
}:
let
  selfLib = self.lib.self;
  flakeConfig = config;
in
{
  flake.furfur = self.nixosConfigurations.furfur.pkgs;

  flake.nixosConfigurations.furfur = inputs.nixpkgs.lib.nixosSystem {
    pkgs = self.configured-pkgs.x86_64-linux.nixpkgs;
    specialArgs = {
      inventoryHostName = "furfur";
    };
    modules = [
      self.nixosModules.furfur-configuration
    ];
  };

  flake.nixosModules.furfur-configuration =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.furfur-configuration";

      imports = [
        self.nixosModules.baseline
        self.nixosModules.srvos-bits

        self.nixosModules.impermanence
        self.nixosModules.disko
        self.nixosModules.systemd-boot
        self.nixosModules.tailscale
        self.nixosModules.impure-nix-setup

        self.nixosModules.microsoft-surface

        self.nixosModules.kanata
        self.nixosModules.niri
        self.nixosModules.firefox
        self.nixosModules.bluetooth
        self.nixosModules.binarin-workstation
        self.nixosModules.binarin-podman
        self.nixosModules.binarin-nix-dev

        self.nixosModules.xpu-smi

        "${self}/my-machines/furfur/hardware-configuration.nix"
      ];

      nixos-config.personal-nix-cache.useHomeNet = false;
      users.users.binarin.extraGroups = [ "i2c" ];
      hardware.i2c.enable = true;

      services.avahi.enable = true;
      nixos-config.export-metrics.enable = true;
      home-manager.users.binarin = self.homeModules.furfur-binarin;

      nixos-config.nix.accessTokens = {
        "github.com" = "extra-access-tokens/github.com";
      };
      system.stateVersion = "25.11";
      hardware.microsoft-surface.kernelVersion = lib.mkForce "longterm";
      networking.hostName = "furfur";
      impermanence.enable = true;

      # services.kanata.keyboards.all.devices = [
      #   "/dev/input/by-path/platform-MSHW0263:00-event-kbd"
      # ];

      # Disable autostart for kanata - start manually with `systemctl start kanata-all`
      systemd.services.kanata-all.wantedBy = lib.mkForce [ ];

      services.logind.settings.Login = {
        HandlePowerKey = "suspend";
        HandlePowerKeyLongPress = "poweroff";

        HandleLidSwitch = "suspend";
        HandleLidSwitchExternalPower = "ignore";
        HandleLidSwitchDocked = "ignore";

        IdleAction = "ignore";
        IdleActionSec = "150";
      };

      boot.kernelPatches = [
        {
          name = "rust-unstable-options";
          patch = pkgs.writeText "rust-unstable-options.patch" ''
diff --git a/arch/x86/Makefile b/arch/x86/Makefile
index 1a27efcf3c20..c99aed37c316 100644
--- a/arch/x86/Makefile
+++ b/arch/x86/Makefile
@@ -76,7 +76,7 @@ export BITS
 #    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=53383
 #
 KBUILD_CFLAGS += -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -mno-avx -mno-sse4a
-KBUILD_RUSTFLAGS += --target=$(objtree)/scripts/target.json
+KBUILD_RUSTFLAGS += -Zunstable-options --target=$(objtree)/scripts/target.json
 KBUILD_RUSTFLAGS += -Ctarget-feature=-sse,-sse2,-sse3,-ssse3,-sse4.1,-sse4.2,-avx,-avx2
 
 #
diff --git a/arch/x86/Makefile.um b/arch/x86/Makefile.um
index c86cbd9cbba3..568bf233947b 100644
--- a/arch/x86/Makefile.um
+++ b/arch/x86/Makefile.um
@@ -12,7 +12,7 @@ KBUILD_CFLAGS +=  -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -mno-avx
 KBUILD_RUSTFLAGS += -Ctarget-feature=-sse,-sse2,-sse3,-ssse3,-sse4.1,-sse4.2,-avx,-avx2
 endif
 
-KBUILD_RUSTFLAGS += --target=$(objtree)/scripts/target.json
+KBUILD_RUSTFLAGS += -Zunstable-options --target=$(objtree)/scripts/target.json
 
 ifeq ($(CONFIG_X86_32),y)
 START := 0x8048000
diff --git a/rust/kernel/irq/request.rs b/rust/kernel/irq/request.rs
index b150563fdef8..33d86255f865 100644
--- a/rust/kernel/irq/request.rs
+++ b/rust/kernel/irq/request.rs
@@ -261,7 +261,7 @@ pub fn synchronize(&self, dev: &Device<Bound>) -> Result {
 /// # Safety
 ///
 /// This function should be only used as the callback in `request_irq`.
-unsafe extern "C" fn handle_irq_callback<T: Handler>(_irq: i32, ptr: *mut c_void) -> c_uint {
+unsafe extern "C" fn handle_irq_callback<T: Handler + 'static>(_irq: i32, ptr: *mut c_void) -> c_uint {
     // SAFETY: `ptr` is a pointer to `Registration<T>` set in `Registration::new`
     let registration = unsafe { &*(ptr as *const Registration<T>) };
     // SAFETY: The irq callback is removed before the device is unbound, so the fact that the irq
@@ -480,7 +480,7 @@ pub fn synchronize(&self, dev: &Device<Bound>) -> Result {
 /// # Safety
 ///
 /// This function should be only used as the callback in `request_threaded_irq`.
-unsafe extern "C" fn handle_threaded_irq_callback<T: ThreadedHandler>(
+unsafe extern "C" fn handle_threaded_irq_callback<T: ThreadedHandler + 'static>(
     _irq: i32,
     ptr: *mut c_void,
 ) -> c_uint {
@@ -496,7 +496,7 @@ pub fn synchronize(&self, dev: &Device<Bound>) -> Result {
 /// # Safety
 ///
 /// This function should be only used as the callback in `request_threaded_irq`.
-unsafe extern "C" fn thread_fn_callback<T: ThreadedHandler>(_irq: i32, ptr: *mut c_void) -> c_uint {
+unsafe extern "C" fn thread_fn_callback<T: ThreadedHandler + 'static>(_irq: i32, ptr: *mut c_void) -> c_uint {
     // SAFETY: `ptr` is a pointer to `ThreadedRegistration<T>` set in `ThreadedRegistration::new`
     let registration = unsafe { &*(ptr as *const ThreadedRegistration<T>) };
     // SAFETY: The irq callback is removed before the device is unbound, so the fact that the irq
'';
        }
      ];

      boot.kernelParams = [
        "i915.enable_psr=0"
      ];

      boot.blacklistedKernelModules = [
        "intel_ipu6"
        "intel_ipu6_isys"
      ];

      fileSystems."/persist".neededForBoot = true;
      fileSystems."/local".neededForBoot = true;

      boot.initrd.luks.devices.luks1.crypttabExtraOpts = [
        "fido2-device=auto"
        "token-timeout=10s"
      ];

      systemd.tmpfiles.settings."10-persistent-ownership" = {
        "/persist/home/binarin".d = {
          user = "binarin";
          group = "binarin";
          mode = "0700";
        };
        "/local/home/binarin".d = {
          user = "binarin";
          group = "binarin";
          mode = "0700";
        };
        "/nix/var/nix/profiles/per-user/binarin".d = {
          user = "binarin";
          group = "binarin";
          mode = "0755";
        };
      };

      sops.secrets.binarin_password_hash.neededForUsers = true;
      users.users.binarin.hashedPasswordFile = config.sops.secrets.binarin_password_hash.path;

      sops.secrets.root_password_hash.neededForUsers = true;
      users.users.root.hashedPasswordFile = config.sops.secrets.root_password_hash.path;

      sops.secrets.agares_password = { };
      sops.templates.networkmanager-env-file.content = ''
        AGARES_PSK=${config.sops.placeholder.agares_password}
      '';

      time.hardwareClockInLocalTime = true;

      services.displayManager = {
        defaultSession = lib.mkForce "niri-uwsm";
      };

      networking.networkmanager.enable = true;
      networking.networkmanager.ensureProfiles = {
        environmentFiles = [
          config.sops.templates.networkmanager-env-file.path
        ];

        profiles = {
          agares = {
            connection = {
              id = "agares";
              type = "wifi";
            };
            ipv4 = {
              method = "auto";
            };
            ipv6 = {
              addr-gen-mode = "stable-privacy";
              method = "auto";
            };
            wifi = {
              mode = "infrastructure";
              ssid = "agares";
            };
            wifi-security = {
              key-mgmt = "wpa-psk";
              psk = "$AGARES_PSK";
            };
          };
        };
      };
    };

  flake.homeModules.furfur-binarin =
    { ... }:
    {
      key = "nixos-config.modules.home.furfur-binarin";

      programs.waybar.battery = {
        enable = true;
        name = "BAT1";
      };
    };

}
