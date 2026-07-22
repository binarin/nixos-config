# SPDX-License-Identifier: MIT
#
# Stupid, non-generic provisioning of the `qdevice-vm` Debian guest on the
# qdevice libvirt host. This replaces the terraform/ definitions with an
# idempotent systemd oneshot that shells out to `virsh`/`virt-install` and
# builds the cloud-init ISO on the target at runtime.
#
# Everything is hardcoded to match terraform/qdevice-vm.tfvars. It is NOT
# meant to be reusable.
{
  lib,
  ...
}:
{
  flake.nixosModules.qdevice-vm =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      # --- Matches terraform/qdevice-vm.tfvars -------------------------------
      vmName = "qdevice-vm";
      memoryMB = 1024;
      vcpu = 1;
      diskGiB = 10; # terraform disk_size = 10 GB
      staticIp = "192.168.2.17";
      gateway = "192.168.2.1";
      dnsServers = [
        "192.168.2.1"
      ];

      poolPath = "/var/lib/libvirt/images";

      # The base cloud image we clone per-VM via a qcow2 backing file.
      baseVolume = "debian-13-genericcloud-amd64.qcow2";
      baseVolumeUrl = "https://cloud.debian.org/images/cloud/trixie/latest/debian-13-genericcloud-amd64.qcow2";
      vmDiskVolume = "${vmName}.qcow2";
      cloudInitIso = "${vmName}-cloudinit.iso";

      # Path on the host where the guest serial console is logged. Lets us
      # observe cloud-init and kernel boot without a Spice/VNC client.
      serialLog = "/var/log/qdevice-vm-console.log";

      # Placeholder used when no root-password secret is configured, so the
      # module still builds and the guest is reachable (then re-provisioned).
      rootPasswordFallback = "change-me";

      # Same authorized_keys source terraform/cloud-init.cfg used. Read on the
      # target at runtime, NOT at nix eval time (the build host may not have it).
      authorizedKeysFile = "/etc/ssh/authorized_keys.d/binarin";

      # Generates the cloud-init NoCloud (cidata) ISO from the live
      # /etc/ssh/authorized_keys file. Built at runtime on qdevice so the keys
      # are always current.
      genCloudInit = pkgs.writeShellApplication {
        name = "gen-${vmName}-cloudinit";
        runtimeInputs = with pkgs; [
          coreutils
          cdrtools
        ];
        # cloud-init interpolates $UPTIME, so it must reach the file literally.
        # Skip shellcheck (SC2016 false-positive on the literal $UPTIME).
        checkPhase = "";
        text = ''
          set -euo pipefail
          out="$1"
          src="$(mktemp -d)"
          trap 'rm -rf "$src"' EXIT

          # Root password for the guest, read from the clan secret var whose
          # path is passed in via the qdevice-vm.rootPasswordFile option. Falls
          # back to a placeholder if unset. This is what makes the serial
          # console (and password SSH) actually usable.
          if [[ -s ${lib.escapeShellArg config.qdevice-vm.rootPasswordFile} ]]; then
            rp="$(cat ${lib.escapeShellArg config.qdevice-vm.rootPasswordFile})"
          else
            rp="${rootPasswordFallback}"
          fi

          # user-data: same shape as terraform/cloud-init.cfg, plus a root
          # password so the serial console is not locked out.
          {
            echo "#cloud-config"
            echo "hostname: ${vmName}"
            echo ""
            echo "users:"
            echo "  - name: root"
            echo "    ssh_authorized_keys:"
            if [[ -s ${lib.escapeShellArg authorizedKeysFile} ]]; then
              while IFS= read -r key; do
                # skip blanks and comments
                case "$key" in
                  ""|\#*) continue ;;
                esac
                printf '      - %s\n' "$key"
              done < ${lib.escapeShellArg authorizedKeysFile}
            fi
            echo ""
            echo "ssh_pwauth: True"
            echo "disable_root: false"
            # Override the cloud image's locked-root default so the password
            # below actually authenticates on the console and over SSH.
            echo "disable_root_opts: '--all'"
            echo ""
            echo "chpasswd:"
            echo "  expire: false"
            echo "  list: |"
            echo "    root:$rp"
            echo ""
            echo "growpart:"
            echo "  mode: auto"
            echo "  devices: [ '/' ]"
            echo ""
            echo "runcmd:"
            echo " - sed -i '/PermitRootLogin/s/.*/PermitRootLogin yes/' /etc/ssh/sshd_config"
            echo " - systemctl restart sshd"
            echo ""
            echo 'final_message: "The system is finally up, after $UPTIME seconds"'
          } > "$src/user-data"

          # meta-data
          {
            echo "instance-id: ${vmName}"
            echo "local-hostname: ${vmName}"
          } > "$src/meta-data"

          # network-config v2 (netplan). NOTE: the interface is named after its
          # PCI address by systemd predictable naming. virtio-net-pci on the q35
          # chipset lands at 0000:01:00.0 => enp1s0, NOT ens3 (ens3 is the
          # virt-install/libvirt default for non-PCI / on-board NICs). Using
          # ens3 here silently matches nothing, leaving the guest without
          # networking -- which was the original bug.
          {
            echo "network:"
            echo "  version: 2"
            echo "  ethernets:"
            echo "    enp1s0:"
            echo "      addresses: [ ${staticIp}/24 ]"
            echo "      routes:"
            echo "        - to: default"
            echo "          via: ${gateway}"
            echo "      nameservers:"
            echo "        addresses: [ ${lib.concatStringsSep "," dnsServers} ]"
          } > "$src/network-config"

          mkisofs -output "$out" \
            -volid cidata -joliet -rock \
            "$src"
        '';
      };

      # libvirt domain XML, matching terraform's libvirt_domain.debian_vm.
      # Baked at build time; paths are fixed (same pool dir as the script uses).
      domainXml = pkgs.writeText "${vmName}.xml" ''
        <domain type='kvm'>
          <name>${vmName}</name>
          <memory unit='MiB'>${toString memoryMB}</memory>
          <vcpu>${toString vcpu}</vcpu>
          <os>
            <type arch='x86_64' machine='q35'>hvm</type>
            <boot dev='hd'/>
          </os>
          <features>
            <acpi/><apic/>
          </features>
          <cpu mode='host-passthrough'/>
          <on_poweroff>destroy</on_poweroff>
          <on_reboot>restart</on_reboot>
          <on_crash>destroy</on_crash>
          <devices>
            <disk type='file' device='disk'>
              <driver name='qemu' type='qcow2'/>
              <source file='${poolPath}/${vmDiskVolume}'/>
              <target dev='vda' bus='virtio'/>
            </disk>
            <disk type='file' device='cdrom'>
              <driver name='qemu' type='raw'/>
              <source file='${poolPath}/${cloudInitIso}'/>
              <target dev='sda' bus='sata'/>
              <readonly/>
            </disk>
            <interface type='bridge'>
              <source bridge='br0'/>
              <model type='virtio'/>
            </interface>
            <console type='file'>
              <source path='${serialLog}' append='on'/>
              <target type='serial' port='0'/>
            </console>
            <serial type='file'>
              <source path='${serialLog}' append='on'/>
              <target type='isa-serial' port='0'>
                <model name='isa-serial'/>
              </target>
            </serial>
            <graphics type='spice' port='5900' autoport='no' listen='127.0.0.1'>
              <listen type='address' address='127.0.0.1'/>
            </graphics>
            <video>
              <model type='virtio'/>
            </video>
            <vsock model='virtio'>
              <cid auto='yes'/>
            </vsock>
          </devices>
        </domain>
      '';

      # The provisioner shell script. Idempotent: re-running is a no-op once
      # the domain exists. Uses the libvirt system connection (qemu:///system).
      # No virt-install/virt-manager dependency -- we define the domain from
      # static XML (above) and start it with plain virsh.
      provisionScript = pkgs.writeShellApplication {
        name = "provision-${vmName}";
        runtimeInputs = with pkgs; [
          libvirt
          qemu
          curl
          coreutils
        ];
        text = ''
          set -euo pipefail

          export VIRSH="virsh --connect qemu:///system"
          POOL_DIR="${poolPath}"
          BASE="${poolPath}/${baseVolume}"
          DISK="${poolPath}/${vmDiskVolume}"
          CIDATA="${poolPath}/${cloudInitIso}"
          BASE_URL="${baseVolumeUrl}"

          # 1. Make sure the storage pool dir exists.
          install -d -m 0755 "$POOL_DIR"

          # 1a. Ensure the serial-console log exists and is writable by the
          # qemu runtime user, so the file-backed <serial> can append to it.
          install -d -m 0755 "$(dirname "${serialLog}")"
          # libvirt runs qemu as the qemu-libvirtd user on this host.
          touch "${serialLog}"
          chown qemu-libvirtd:qemu-libvirtd "${serialLog}" 2>/dev/null || true
          chmod 0644 "${serialLog}"

          # 2. Ensure the 'default' dir-backed pool exists and is running.
          if ! $VIRSH pool-info default >/dev/null 2>&1; then
            $VIRSH pool-define-as --type dir --name default --target "$POOL_DIR"
            $VIRSH pool-build default || true
          fi
          $VIRSH pool-start default >/dev/null 2>&1 || true
          $VIRSH pool-autostart default >/dev/null 2>&1 || true
          $VIRSH pool-refresh default >/dev/null 2>&1 || true

          # 3. Fetch the Debian cloud base image if missing.
          if [[ ! -s "$BASE" ]]; then
            echo "Downloading Debian base image to $BASE ..."
            tmp="''${BASE}.tmp"
            curl -fsSL -o "$tmp" "$BASE_URL"
            mv "$tmp" "$BASE"
            $VIRSH pool-refresh default >/dev/null 2>&1 || true
          fi

          # 4. Create the per-VM disk as a qcow2 backed by the base image, if missing.
          if [[ ! -s "$DISK" ]]; then
            echo "Creating VM disk $DISK ..."
            qemu-img create -f qcow2 \
              -F qcow2 -b "$BASE" \
              "$DISK" ${toString diskGiB}G
          fi

          # 5. (Re)generate the cloud-init ISO from the current authorized_keys.
          ${lib.getExe genCloudInit} "$CIDATA"

          # 6. If the domain already exists, just make sure it is running and bail.
          if $VIRSH dominfo ${vmName} >/dev/null 2>&1; then
            echo "Domain ${vmName} already exists; ensuring it is running."
            $VIRSH start ${vmName} >/dev/null 2>&1 || true
            exit 0
          fi

          # 7. Otherwise define the domain from static XML and start it.
          echo "Defining domain ${vmName} from ${domainXml} ..."
          $VIRSH define ${lib.escapeShellArg domainXml}
          $VIRSH autostart ${vmName}
          $VIRSH start ${vmName}
        '';
      };
    in
    {
      options.qdevice-vm.rootPasswordFile = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = ''
          Path to a file containing the plaintext root password for the
          qdevice-vm guest, used to seed cloud-init's `chpasswd`. Typically
          the `path` of a clan core vars generator file. When empty or the
          file is missing, a placeholder password is used instead.
        '';
      };

      config = {
        # Make mkisofs available on the host (also used by the generator above).
        environment.systemPackages = [ pkgs.cdrtools ];

        # Run once per boot, after libvirtd is up. Remains "active" so a second
        # activation while running just re-checks state.
        systemd.services.qdevice-vm = {
          description = "Provision the qdevice-vm Debian guest on libvirt";
          after = [
            "libvirtd.service"
            "network-online.target"
          ];
          wants = [ "network-online.target" ];
          requires = [ "libvirtd.service" ];
          wantedBy = [ "multi-user.target" ];
          path = [
            pkgs.libvirt
            pkgs.qemu
            pkgs.cdrtools
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = lib.getExe provisionScript;
          };
        };
      };
    };
}
