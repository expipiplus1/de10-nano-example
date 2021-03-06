{ stdenv, fetchgit, autoconf, autoconf-archive, automake, libtool, pkgconfig, kmod }:

stdenv.mkDerivation rec {
  name = "libgpiod-${version}";
  version = "2018-10-07";

  src = fetchgit {
    url = "git://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git";
    rev = "4bf402d3a49336eacd33654441d575bd267780b8";
    sha256 = "01f3jzb133z189sxdiz9qiy65p0bjqhynfllidbpxdr0cxkyyc1d";
  };

  buildInputs = [ kmod ];
  nativeBuildInputs = [
    autoconf
    autoconf-archive
    automake
    libtool
    pkgconfig
  ];

  configurePhase = ''
    ./autogen.sh \
      --enable-tools=yes \
      --enable-bindings-cxx \
      --prefix="$out"
  '';

  meta = with stdenv.lib; {
    description = "C library and tools for interacting with the linux GPIO character device";
    longDescription = ''
      Since linux 4.8 the GPIO sysfs interface is deprecated. User space should use
      the character device instead. This library encapsulates the ioctl calls and
      data structures behind a straightforward API.
    '';
    homepage = https://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git/about/;
    license = licenses.lgpl2;
    maintainers = [ maintainers.expipiplus1 ];
    platforms = platforms.linux;
  };
}
