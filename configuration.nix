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
    (pkgsPath + "/nixos/modules/installer/cd-dvd/sd-image.nix")

    # Enable devices which are usually scanned, because we don't know the
    # target system.
    (pkgsPath + "/nixos/modules/installer/scan/detected.nix")
    (pkgsPath + "/nixos/modules/installer/scan/not-detected.nix")
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
            { name = "de10-nano";
              patch = (pkgs.fetchFromGitHub {
                owner = "intel";
                repo = "meta-de10-nano";
                rev = "9ad8a88e1d1181ba6f54c79ecebb81c1d68042ee";
                sha256 = "1111111111111111111111111111111111111111111111111111111111111111";
              }) + "/recipes-kernel/linux/config/socfpga-4.1-ltsi/patches/0001-Add-DE10-Nano-devicetree.patch";
            }
          ];
          defconfig = "socfpga_defconfig";
        };
    in  pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor linux_socfpga);

  system.fsPackages = [ pkgs.dosfstools ];

  boot.initrd.kernelModules = [ "vfat" "ip_tables" "nls-cp437" ];

  # Disable some other stuff we don't need.
  security.sudo.enable = false;

  # Automatically log in at the virtual consoles.
  services.mingetty.autologinUser = "root";

  # Allow sshd to be started manually through "systemctl start sshd".
  services.openssh = {
    enable = true;
    # Allow password login to the installation, if the user sets a password via "passwd"
    # It is safe as root doesn't have a password by default and SSH is disabled by default
    permitRootLogin = "yes";
  };

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

  # Allow the user to log in as root without a password.
  users.users.root.initialHashedPassword = "";

  # systemd.timers."joe-debug" =
  #                     {
  #                       wantedBy = [ "timers.target" ];
  #                       timerConfig = {
  #                         OnCalendar = "10 s";
  #                         Persistent = "yes";
  #                       };
  #                     };

  systemd.services."joe-debug" = {
    description = "debug echo";
    startAt = "*:*:0/3";
    script = ''
      echo hello > /hello
      sync
      # systemctl status >> /status
    '';
  };

  sdImage = {
    populateBootCommands = let
      configTxt = pkgs.writeText "config.txt" ''
        # Prevent the firmware from smashing the framebuffer setup done by the mainline kernel
        # when attempting to show low-voltage or overtemperature warnings.
        avoid_warnings=1

        # U-Boot used to need this to work, regardless of whether UART is actually used or not.
        # TODO: check when/if this can be removed.
        enable_uart=1
      '';
      in ''
        cp ${configTxt} boot/config.txt
        ${extlinux-conf-builder} -t 3 -c ${config.system.build.toplevel} -d ./boot
      '';
  };
}
