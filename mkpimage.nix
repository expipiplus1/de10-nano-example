{ pkgs, fpga-family, u-boot-spl }:
let soc-eds = import ./soc-eds.nix { inherit pkgs; };
in pkgs.stdenv.mkDerivation rec {
  name = "signed-uboot-image";
  unpackPhase = ":";
  buildPhase = ''
    cp "${u-boot-spl}" ./u-boot-spl.bin

    # --header-version: Header version to be created
    # (Arria/Cyclone V = 0, Arria 10 = 1)

    echo "### Running mkpimage"
    ${soc-eds}/embedded/host_tools/altera/mkpimage/mkpimage \
      --header-version ${if fpgaFamily == "arria-10" then "1" else "0"} \
      --output u-boot-spl-mkpimage.bin \
      u-boot-spl.bin \
      u-boot-spl.bin \
      u-boot-spl.bin \
      u-boot-spl.bin
  '';
  installPhase = ''
    mkdir -p "$out"
    mv u-boot-spl-mkpimage.bin "$out"
  '';
}
