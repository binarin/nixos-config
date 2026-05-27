{ self, inputs, ... }:
let
  selfLib = self.lib.self;
in
{
  flake-file.inputs = {
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  flake.nixosModules.microsoft-surface =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.microsoft-surface";
      imports = [
        inputs.nixos-hardware.nixosModules.microsoft-surface-pro-intel
      ];

      hardware.microsoft-surface.kernelVersion = "stable";
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

      boot.initrd.kernelModules = [
        "surface_aggregator"
        "surface_aggregator_registry"
        "surface_aggregator_hub"
        "surface_aggregator_tabletsw"
        "surface_hid_core"
        "surface_hid"
        "8250_dw"
        "intel_lpss_pci"
        "intel_lpss"
        "pinctrl_tigerlake"
      ];
    };
}
