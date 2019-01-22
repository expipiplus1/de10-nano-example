let pkgsPath = import ./pkgs.nix;
    system = "armv7l-linux";
    nixos = import (pkgsPath + "/nixos");
    configuration = ./configuration.nix;
    config = (nixos { inherit system configuration; }).config;

    storePaths = [ config.system.build.toplevel ];
    rootfsImage = import (pkgsPath + "/nixos/lib/make-ext4-fs.nix") {
      inherit pkgs storePaths;
      volumeLabel = "NIXOS_SD";
      inherit (pkgs) e2fsprogs libfaketime perl;
    };

    crossPkgs = import pkgsPath {
      crossSystem = {
        system = "armv7l-linux";
      };
    };

    pkgs = import pkgsPath {
    };

    libgpiod = pkgs.callPackage ./libgpiod.nix {};

    mkpimage = import ./tools/mkpimage.nix {inherit pkgs;};

    sopc2dts = import ./sopc2dts.nix {inherit pkgs;};

    soc-eds = import ./soc-eds.nix { inherit pkgs; };

    extlinux-conf-builder = import (pkgsPath + "/nixos/modules/system/boot/loader/generic-extlinux-compatible/extlinux-conf-builder.nix") {
      inherit pkgs;
    };

    u-boot = crossPkgs.buildUBoot rec {
        defconfig = "socfpga_de10_nano_defconfig";
        extraMeta.platforms = [ "armv7l-linux" ];
        filesToInstall = [ "u-boot-with-spl.sfp" "spl/u-boot-spl.bin" ];
      };

    u-boot-image = u-boot + "/u-boot-with-spl.sfp";

    image = pkgs.stdenv.mkDerivation rec {
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

      bootSize = 120;
      buildCommand = ''
        ubootSizeBlocks=$(du -B 512 --apparent-size "${u-boot-image}" | awk '{ print $1 }')
        bootSizeBlocks=$((${toString bootSize} * 1024 * 1024 / 512))
        rootSizeBlocks=$(du -B 512 --apparent-size ${rootfsImage} | awk '{ print $1 }')
        imageSize=$((rootSizeBlocks * 512 + bootSizeBlocks * 512 + ubootSizeBlocks * 512 + 20 * 1024 * 1024))
        truncate -s "$imageSize" "$out"

        # type=a2 is an Intel devkit specific magic number
        # type=b is 'W95 FAT32'
        # type=83 is 'Linux'
        sfdisk $out <<EOF
            label: dos
            label-id: 0x2178694e

            start=2M, size=$ubootSizeBlocks, type=a2
            start=8M, size=$bootSizeBlocks, type=b, bootable
            start=${toString (8 + bootSize)}M, type=83
        EOF

        #
        # Copy uboot into the SD image
        #
        eval $(partx $out -o START,SECTORS --nr 1 --pairs)
        dd conv=notrunc if="${u-boot-image}" of="$out" seek=$START count=$SECTORS

        #
        # Create a FAT32 /boot partition of suitable size into bootpart.img
        #
        eval $(partx $out -o START,SECTORS --nr 2 --pairs)
        truncate -s $((SECTORS * 512)) bootpart.img
        faketime "1970-01-01 00:00:00" mkfs.vfat -i 0x2178694e -n NIXOS_BOOT bootpart.img
        # Populate the files intended for /boot
        mkdir boot
        ${extlinux-conf-builder} -t 3 -c ${config.system.build.toplevel} -d ./boot
        # Copy the populated /boot into the SD imag
        (cd boot; mcopy -bpsvm -i ../bootpart.img ./* ::)
        dd conv=notrunc if=bootpart.img of=$out seek=$START count=$SECTORS

        #
        # Copy the rootfs into the SD image
        #
        eval $(partx $out -o START,SECTORS --nr 3 --pairs)
        dd conv=notrunc if=${rootfsImage} of=$out seek=$START count=$SECTORS
      '';
    };
in {
  inherit rootfsImage u-boot image soc-eds sopc2dts storePaths libgpiod;
  crossGcc = pkgs.gcc-arm-embedded;
}
