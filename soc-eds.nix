{ pkgs }:

with pkgs;

let
  setup-chroot-and-exec = writeScript "setup-chroot-and-exec"
    (''
      #!${bash}/bin/sh
      chrootdir=chroot  # relative to the current directory
      mkdir -p "$chrootdir"/host
      mkdir -p "$chrootdir"/proc
      mkdir -p "$chrootdir"/nix
      mkdir -p "$chrootdir"/tmp
      mkdir -p "$chrootdir"/dev
      mkdir -p "$chrootdir"/lib
      mkdir -p "$chrootdir"/lib64
      mkdir -p "$chrootdir"/bin
      ${utillinux}/bin/mount --rbind /     "$chrootdir"/host
      ${utillinux}/bin/mount --rbind /proc "$chrootdir"/proc
      ${utillinux}/bin/mount --rbind /nix  "$chrootdir"/nix
      ${utillinux}/bin/mount --rbind /tmp  "$chrootdir"/tmp
      ${utillinux}/bin/mount --rbind /dev  "$chrootdir"/dev
    '' + (if false then ''
      ${utillinux}/bin/mount --rbind "${glibc_lib32}"/lib "$chrootdir"/lib
    '' else ''
      ${utillinux}/bin/mount --rbind "${glibc}"/lib64 "$chrootdir"/lib64
    '') + ''
      ${utillinux}/bin/mount --rbind "${bash}"/bin "$chrootdir"/bin
      chroot "$chrootdir" "$@"
    '');

  installer = fetchurl {
    url = "http://download.altera.com/akdlm/software/acdsinst/18.1std/625/ib_installers/SoCEDSSetup-18.1.0.625-linux.run";
    sha256 = "0zrliw930brsj511cl2n3vdl7pr31dxl8zxh559bpks0ncwc33j9";
    # sha256 = "1w1a4q09myg8l9n0x1js1qfb9dgsb6lrrgp31aq6knbgmx6zjkhq";
    executable = true;
  };

  # buildFHSUserEnv from nixpkgs tries to mount a few directories that are not
  # available in sandboxed Nix builds (/sys, /run), hence we have our own
  # slimmed down variant.
  run-in-fhs-env = writeScript "run-in-fhs-env"
    ''
      #!${bash}/bin/sh
      if [ "$*" = "" ]; then
          echo "Usage: run-in-fhs-env <COMMAND> [ARGS...]"
          exit 1
      fi
      "${utillinux}/bin/unshare" -r -U -m "${setup-chroot-and-exec}" "$@"
    '';


    runtimeLibPath =
      with pkgs.xorg;
      stdenv.lib.makeLibraryPath
      [ zlib glib libpng12 freetype libSM libICE libXrender fontconfig.lib
        libXext libX11 libXtst libXi gtk2 bzip2.out libelf
        stdenv.cc.cc.lib
      ];

    soc-eds = stdenv.mkDerivation rec {
      name = "SoCEDS";

      srcs = [];

      unpackPhase = ":";

      dontBuild = true;

      buildInputs = [ file nukeReferences ];

			# Prebuilt binaries need special treatment
			dontStrip = true;
			dontPatchELF = true;

    # Quartus' setup.sh (from the all-in-one-installers) doesn't fit our needs
    # (we want automatic and distro-agnostic install), so call the actual setup
    # program directly instead.
    #
    # Quartus*Setup*.run files are statically linked ELF executables that run
    # open("/lib64/ld-linux-x86-64.so.2", ...) (or "/lib/ld-linux.so.2" for
    # 32-bit versions) . That obviously doesn't work in sandboxed Nix builds.
    #
    # Things that do not work:
    # * patchelf the installer (there is no .interp section in static ELF)
    # * dynamic linker tricks (again, static ELF)
    # * proot (the installer somehow detects something is wrong and aborts)
    #
    # We need bigger guns: user namespaces and chroot. That's how we make /lib64/
    # available to the installer. The installer installs dynamically linked ELF
    # files, so those we can fixup with usual tools.
    #
    # For runtime, injecting (or wrapping with) LD_LIBRARY_PATH is easier, but it
    # messes with the environment for all child processes. We take the less
    # invasive approach here, patchelf + RPATH. Unfortunately, Quartus itself
    # uses LD_LIBRARY_PATH in its wrapper scripts. This cause e.g. firefox to
    # fail due to LD_LIBRARY_PATH pulling in wrong libraries for it (happens if
    # clicking any URL in Quartus).
    installPhase = ''
      run_quartus_installer()
      {
          installer="$1"
          if [ ! -x "$installer" ]; then
              echo "ERROR: \"$installer\" either doesn't exist or is not executable"
              exit 1
          fi
          echo "### ${run-in-fhs-env} $installer --mode unattended --installdir $out --accept_eula 1"
          "${run-in-fhs-env}" "$installer" --mode unattended --installdir "$out" --accept_eula 1
          echo "...done"
      }

      echo "Running Quartus Setup (in FHS sandbox)..."
      run_quartus_installer "${installer}"

      echo "Removing unneeded \"uninstall\" binaries (saves $(du -sh "$out"/uninstall | cut -f1))"
      rm -rf "$out"/uninstall

      echo "Prevent retaining a runtime dependency on the installer binaries (saves $(du -sh "${installer}" | cut -f1))"
      nuke-refs "$out/logs/"*

      echo "Fixing ELF interpreter paths with patchelf"
      find "$out" -type f | while read f; do
          case "$f" in
              *.debug) continue;;
          esac
          # A few files are read-only. Make them writeable for patchelf. (Nix
          # will make all files read-only after the build.)
          chmod +w "$f"
          file_magic=$(file "$f") || { echo "file \"$f\" failed"; exit 1; }
          case "$file_magic" in
              *ELF*dynamically\ linked*)
                  # Filter out some /build absolute paths in the distrubuted objects...
                  orig_rpath=$(patchelf --print-rpath "$f" | sed -r -e 's|(^\|:)(/(build\|data\|tools)/[^:]*:?)+|\1|g' -e 's|:$||') || { echo "FAILED: patchelf --print-rpath $f"; exit 1; }
                  # Take care not to add ':' at start or end of RPATH, because
                  # that is the same as '.' (current directory), and that's
                  # insecure.
                  if [ "$orig_rpath" != "" ]; then
                      orig_rpath="$orig_rpath:"
                  fi
                  new_rpath="$orig_rpath${runtimeLibPath}"
                  magic=$(readelf -h "$f" | grep '^  Type:') || { echo "readelf \"$f\" failed"; exit 1; }
                  case "$magic" in
                      *EXEC*Executable\ file*)
                          interp=$(patchelf --print-interpreter "$f") || { echo "FAILED: patchelf --print-interpreter $f"; exit 1; }
                          # Note the LSB interpreters, required by some files
                          case "$interp" in
                              /lib64/ld-linux-x86-64.so.2|/lib64/ld-lsb-x86-64.so.3)
                                  new_interp=$(cat "$NIX_CC"/nix-support/dynamic-linker)
                                  ;;
                              /lib/ld-linux.so.2|/lib/ld-lsb.so.3)
                                  new_interp="${pkgsi686Linux.glibc}/lib/ld-linux.so.2"
                                  ;;
                              /lib/ld-linux-armhf.so.3|/lib64/ld64.so.1|/lib64/ld64.so.2)
                                  # Ignore ARM/ppc64/ppc64le executables, they
                                  # are not meant to be run on the build machine.
                                  # Example files:
                                  #   altera-quartus-prime-lite-15.1.0.185/hld/host/arm32/bin/aocl-binedit
                                  #   altera-quartus-prime-lite-15.1.0.185/hld/host/ppc64/bin/aocl-binedit
                                  #   altera-quartus-prime-lite-15.1.0.185/hld/host/ppc64le/bin/aocl-binedit
                                  continue
                                  ;;
                              *)
                                  echo "FIXME: unhandled interpreter \"$interp\" in $f"
                                  exit 1
                                  ;;
                          esac
                          test -f "$new_interp" || { echo "$new_interp is missing"; exit 1; }
                          patchelf --set-interpreter "$new_interp" \
                                   --set-rpath "$new_rpath" "$f" || { echo "FAILED: patchelf --set-interpreter $new_interp --set-rpath $new_rpath $f"; exit 1; }
                          ;;
                      *DYN*Shared\ object\ file*)
                          patchelf --set-rpath "$new_rpath" "$f" || { echo "FAILED: patchelf --set-rpath $new_rpath $f"; }
                          ;;
                  esac
                  ;;
              *ELF*statically\ linked*)
                  echo "WARN: $f is statically linked. Needs fixup?"
                  ;;
          esac
      done

      # Modelsim is optional
      f="$out"/modelsim_ase/vco
      if [ -f "$f" ]; then
          echo "Fix hardcoded \"/bin/ls\" in .../modelsim_ase/vco"
          sed -i -e "s,/bin/ls,ls," "$f"

          echo "Fix support for Linux 4.x in .../modelsim_ase/vco"
          sed -i -e "/case \$utype in/a 4.[0-9]*) vco=\"linux\" ;;" "$f"
      fi
    '';
    };

in soc-eds

