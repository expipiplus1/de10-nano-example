let pkgsPath = import ./pkgs.nix;
    system = "armv7l-linux";
    nixos = import (pkgsPath + "/nixos");
    configuration = ./configuration.nix;
    config = (nixos { inherit system configuration; }).config;

    rootfsImage = import (pkgsPath + "/nixos/lib/make-ext4-fs.nix") {
      inherit pkgs;
      inherit (config.sdImage) storePaths;
      volumeLabel = "NIXOS_SD";
      inherit (pkgs) e2fsprogs libfaketime perl;
    };

    crossPkgs = import config.nixpkgs.pkgs.path {
      crossSystem = {
        system = "armv7l-linux";
      };
    };

    pkgs = import config.nixpkgs.pkgs.path {
    };

    mkpimage = import ./tools/mkpimage.nix {inherit pkgs;};

    extlinux-conf-builder = import (pkgsPath + "/nixos/modules/system/boot/loader/generic-extlinux-compatible/extlinux-conf-builder.nix") {
      inherit pkgs;
    };

    soc-eds = import ./soc-eds.nix { inherit pkgs; };

    u-boot = crossPkgs.buildUBoot {
      defconfig = "socfpga_de10_nano_defconfig";
      extraMeta.platforms = [ "armv7l-linux" ];
      filesToInstall = [ "u-boot.img" ];
    };

    preloader = import ./u-boot-spl.nix { inherit pkgs; };
    preloaderImage = preloader + "/preloader-mkpimage.bin";

    image = pkgs.stdenv.mkDerivation {
      name = "cyclone-v-sd.img";

      buildInputs = with pkgs; [
        dosfstools
        e2fsprogs
        libfaketime
        mtools
        utillinux
      ];

      # This is just stitching large files together, no need to transmit them too
      # and fro.
      preferLocalBuild = true;

      buildCommand = ''
        # Create the image file sized to fit /boot and /, plus 20M of slack
        rootSizeBlocks=$(du -B 512 --apparent-size ${rootfsImage} | awk '{ print $1 }')
        bootSizeBlocks=$((${toString config.sdImage.bootSize} * 1024 * 1024 / 512))
        ubootSizeBlocks=$(du -B 512 --apparent-size ${preloaderImage} | awk '{ print $1 }')
        imageSize=$((rootSizeBlocks * 512 + bootSizeBlocks * 512 + ubootSizeBlocks * 512 + 20 * 1024 * 1024))
        truncate -s $imageSize $out

        # This boot partition must be first!
        # type=b is 'W95 FAT32'
        # type=a2 is an Arria10 specific magic number
        # type=83 is 'Linux'
        sfdisk $out <<EOF
            label: dos
            label-id: 0x2178694e

            start=8M, size=$bootSizeBlocks, type=b, bootable
            start=${toString (8 + config.sdImage.bootSize)}M, size=$ubootSizeBlocks, type=a2
            start=${toString (10 + config.sdImage.bootSize)}M, type=83
        EOF

        # Copy the rootfs into the SD image
        eval $(partx $out -o START,SECTORS --nr 3 --pairs)
        dd conv=notrunc if=${rootfsImage} of=$out seek=$START count=$SECTORS

        # Create a FAT32 /boot partition of suitable size into bootpart.img
        eval $(partx $out -o START,SECTORS --nr 1 --pairs)
        truncate -s $((SECTORS * 512)) bootpart.img
        faketime "1970-01-01 00:00:00" mkfs.vfat -i 0x2178694e -n NIXOS_BOOT bootpart.img

        # Populate the files intended for /boot
        mkdir boot
        ${""
        # cp ${linux}/zImage ./boot
        # cp ${linux-device-tree} ./boot/socfpga_arria10_socdk_sdmmc.dtb
        # cp ${rbf}/socfpga.periph.rbf ./boot
        # cp ${rbf}/socfpga.core.rbf ./boot
        # ls -lah boot/nixos
        }
        mkdir -p "./boot/de10-nano-base"
        cp ${u-boot}/u-boot.img ./boot/de10-nano-base/
        ${extlinux-conf-builder} -t 3 -c ${config.system.build.toplevel} -d ./boot

        # Copy the populated /boot into the SD image
        (cd boot; mcopy -bpsvm -i ../bootpart.img ./* ::)
        dd conv=notrunc if=bootpart.img of=$out seek=$START count=$SECTORS

        # Copy uboot into the SD image
        eval $(partx $out -o START,SECTORS --nr 2 --pairs)
        dd conv=notrunc if=${preloaderImage} of=$out seek=$START count=$SECTORS
      '';
    };
in {
  inherit rootfsImage u-boot image soc-eds preloader;
  inherit (config.sdImage) storePaths;
}
