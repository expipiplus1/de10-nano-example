# To build, use:
# nix-build nixos -I nixos-config=nixos/modules/installer/cd-dvd/sd-image-armv7l-multiplatform.nix -A config.system.build.sdImage
{ config, lib, pkgs, ... }:

let
  pkgsPath = import ./pkgs.nix;

  extlinux-conf-builder =
    import (pkgsPath + "/nixos/modules/system/boot/loader/generic-extlinux-compatible/extlinux-conf-builder.nix") {
      inherit pkgs;
    };

in {
  assertions = lib.singleton {
    assertion = pkgs.stdenv.system == "armv7l-linux";
    message = "sd-image-armv7l-multiplatform.nix can be only built natively on ARMv7; " +
      "it cannot be cross compiled";
  };

  imports = [
    ./hardware-configuration.nix
    (pkgsPath + "/nixos/modules/profiles/minimal.nix")

    # Allow "nixos-rebuild" to work properly by providing
    # /etc/nixos/configuration.nix.
    # (pkgsPath + "/nixos/modules/profiles/clone-config.nix")

    # Include a copy of Nixpkgs so that nixos-install works out of
    # the box.
    # (pkgsPath + "/nixos/modules/installer/cd-dvd/channel.nix")
  ];

  environment.systemPackages = [
    pkgs.vim
    pkgs.tmux
    pkgs.file
    (pkgs.callPackage ./libgpiod.nix {})
    pkgs.devmem2
  ];

  sound.enable = false;
  services.udisks2.enable =  false;

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.consoleLogLevel = lib.mkDefault 7;
  boot.kernelParams = ["console=ttyS0,115200n8"];

  boot.kernelPackages =
    let linux_socfpga = pkgs.callPackage ./linux-socfpga.nix {
          kernelPatches = [
            pkgs.kernelPatches.bridge_stp_helper
            # See pkgs/os-specific/linux/kernel/cpu-cgroup-v2-patches/README.md
            # when adding a new linux version
            # kernelPatches.cpu-cgroup-v2."4.11"
            pkgs.kernelPatches.modinst_arg_list_too_long
            { name = "de10-nano-devicetree";
              patch = ./de10-nano-devicetree.patch;
            }
          ];
          defconfig = "socfpga_defconfig";
          structuredExtraConfig =
            let y = "y";
                n = "n";
            in {
              OF_OVERLAY = y;
              OF_CONFIGFS = y;

              LBDAF = y;

              PCI = n;
              WLAN = n;

              USB_CONFIGFS = y;
              USB_CONFIGFS_MASS_STORAGE = y;
              USB_CONFIGFS_RNDIS = y;

            # LBDAF = "y";

            # OF_OVERLAY = "y";
            # OF_CONFIGFS = "y";


            # USB_GADGET_DEBUG = "y";
            # USB_GADGET_VERBOSE = "y";
            # USB_GADGET_DEBUG_FILES = "y";
            # USB_GADGET_DEBUG_FS = "y";
            # U_SERIAL_CONSOLE = "y";
            # USB_FUSB300 = "y";
            # USB_FOTG210_UDC = "y";
            # USB_GR_UDC = "y";
            # USB_R8A66597 = "y";
            # USB_PXA27X = "y";
            # USB_MV_UDC = "y";
            # USB_MV_U3D = "y";
            # USB_SNP_CORE = "y";
            # USB_SNP_UDC_PLAT = "y";
            # USB_M66592 = "y";
            # USB_BDC_UDC = "y";
            # USB_BDC_PCI = "y";
            # USB_AMD5536UDC = "y";
            # USB_NET2272 = "y";
            # USB_NET2272_DMA = "y";
            # USB_NET2280 = "y";
            # USB_GOKU = "y";
            # USB_EG20T = "y";
            # USB_GADGET_XILINX = "y";
            # USB_DUMMY_HCD = "y";
            # USB_LIBCOMPOSITE = "y";
            # USB_F_ACM = "y";
            # USB_U_SERIAL = "y";
            # USB_U_ETHER = "y";
            # USB_F_OBEX = "y";
            # USB_F_NCM = "y";
            # USB_F_ECM = "y";
            # USB_F_EEM = "y";
            # USB_F_SUBSET = "y";
            # USB_F_RNDIS = "y";
            # USB_F_FS = "y";
            # USB_CONFIGFS = "y";
            # USB_CONFIGFS_ACM = "y";
            # USB_CONFIGFS_OBEX = "y";
            # USB_CONFIGFS_NCM = "y";
            # USB_CONFIGFS_ECM = "y";
            # USB_CONFIGFS_ECM_SUBSET = "y";
            # USB_CONFIGFS_RNDIS = "y";
            # USB_CONFIGFS_EEM = "y";
            # USB_CONFIGFS_F_FS = "y";
            # USB_ETH = "y";
            # USB_ETH_RNDIS = "y";
            # USB_ETH_EEM = "y";
          };
        };
    in  pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor linux_socfpga);

  system.fsPackages = [ pkgs.dosfstools ];

  boot.initrd.kernelModules = [ "vfat" ];

  # Automatically log in at the virtual consoles.
  services.mingetty.autologinUser = "root";

  # Allow sshd to be started manually through "systemctl start sshd".
  services.openssh.enable = true;

  services.dhcpd4 = {
    enable = true;
    interfaces = [ "usb0" ];
    extraConfig = ''
      option subnet-mask 255.255.255.0;
      option broadcast-address 192.168.7.255;
      subnet 192.168.7.0 netmask 255.255.255.0 {
        range 192.168.7.6 192.168.7.7;
      }
    '';
  };

  systemd.services.rndis = {
      description = "Bring up RNDIS host";
      before = [ "network.target" ];
      wantedBy = [ "network.target" ];
      after = [ "network-pre.target" ];
      script = ''
        cd /sys/kernel/config

        mkdir usb_gadget/g1
        cd usb_gadget/g1

        echo 0x0525 > idVendor
        echo 0xa4a2 > idProduct
        mkdir strings/0x409
        echo Linux > strings/0x409/manufacturer
        echo "Etherned/RNDIS gadget" > strings/0x409/product

        mkdir configs/c.1
        echo 100 > configs/c.1/MaxPower
        mkdir configs/c.1/strings/0x409
        echo "RNDIS" > configs/c.1/strings/0x409/configuration

        mkdir functions/rndis.usb0 # use default parameters

        ln -s functions/rndis.usb0 configs/c.1

        echo ffb40000.usb > UDC
      '';
  };


  networking.interfaces.usb0.ipv4.addresses = [ { address = "192.168.7.1"; prefixLength = 24; } ];
  networking.dhcpcd.denyInterfaces = [ "usb0" ];
  networking.defaultGateway = { address = "192.168.7.6"; interface = "usb0"; };

  # Tell the Nix evaluator to garbage collect more aggressively.
  # This is desirable in memory-constrained environments that don't
  # (yet) have swap set up.
  environment.variables.GC_INITIAL_HEAP_SIZE = "100000";

  # Make the installer more likely to succeed in low memory
  # environments.  The kernel's overcommit heustistics bite us
  # fairly often, preventing processes such as nix-worker or
  # download-using-manifests.pl from forking even if there is
  # plenty of free memory.
  boot.kernel.sysctl."vm.overcommit_memory" = "1";

  security.polkit.enable = false;

  nix.trustedUsers = [ "root" "@wheel" ];

  users.mutableUsers = false;

  users.users.j = {
    isNormalUser = true;
    home = "/home/j";
    description = "Joe Hermaszewski";
    extraGroups = [ "wheel" ];
    hashedPassword = "$6$22Tois4OjFC$y3kfcuR7BBHVj8LnZNIfLyNhQOdVZkkTseXCNbiA95WS2JSXv4Zynmy8Ie9nCxNokgSL8cuO1Le0m4VHuzXXI.";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDFErWB61gZadEEFteZYWZm8QRwabpl4kDHXsm0/rsLqoyWJN5Y4zF4kowSGyf92LfJu9zNBs2viuT3vmsLfg6r4wkbVyujpEo3JLuV79r9K8LcM32wA52MvQYATEzxuamZPZCBT9fI/2M6bC9lz67RQ5IoENfjZVCstOegSmODmOvGUs6JjrB40slB+4YXCVFypYq3uTyejaBMtKdu1S4TWUP8WRy8cWYmCt1+a6ACV2yJcwnhSoU2+QKt14R4XZ4QBSk4hFgiw64Bb3WVQlfQjz3qA4j5Tc8P3PESKJcKW/+AsavN1I2FzdiX1CGo2OL7p9TcZjftoi5gpbmzRX05 j@riza"
    ];
  };

  boot.postBootCommands = ''
  # On the first boot do some maintenance tasks
  if [ -f /nix-path-registration ]; then
    # Figure out device names for the boot device and root filesystem.
    rootPart=$(readlink -f /dev/disk/by-label/NIXOS_SD)
    bootDevice=$(lsblk -npo PKNAME $rootPart)

    # Resize the root partition and the filesystem to fit the disk
    echo ",+," | sfdisk -N3 --no-reread $bootDevice
    ${pkgs.parted}/bin/partprobe
    ${pkgs.e2fsprogs}/bin/resize2fs $rootPart

    # Register the contents of the initial Nix store
    ${config.nix.package.out}/bin/nix-store --load-db < /nix-path-registration

    # nixos-rebuild also requires a "system" profile and an /etc/NIXOS tag.
    touch /etc/NIXOS
    ${config.nix.package.out}/bin/nix-env -p /nix/var/nix/profiles/system --set /run/current-system

    # Prevents this from running on later boots.
    rm -f /nix-path-registration
  fi
'';

}
