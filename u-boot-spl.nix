{ pkgs }:

with pkgs;

let
  release = import ./de10-nano-hardware-release.nix { inherit pkgs; };
  soc-eds = import ./soc-eds.nix { inherit pkgs; };

  mkpimage = soc-eds + "/embedded/host_tools/altera/mkpimage";

  version = "2018.07";
  u-boot-src = fetchurl {
    url = "ftp://ftp.denx.de/pub/u-boot/u-boot-${version}.tar.bz2";
    sha256 = "1m7nw64mxflpc6sqvnz2kb5fxfkb4mrpy8b1wi15dcwipj4dy44z";
  };

in
  stdenv.mkDerivation {
    name = "u-boot-spl";
    SOCEDS_DEST_ROOT = soc-eds + "/embedded";
    src = release;
    buildPhase = ''
      export PATH=$PATH:${mkpimage}:${gcc-arm-embedded-6}/bin
      cd de10-nano-base/preloader
      make CROSS_COMPILE=arm-none-eabi-
    '';
    installPhase = ''
      mkdir -p "$out"
      mv preloader-mkpimage.bin "$out"
    '';
  }
